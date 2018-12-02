{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Interface for GraphQL API.
--
-- __Note__: This module is highly subject to change. We're still figuring
-- where to draw the lines and what to expose.
module GraphQL
  (
    -- * Running queries
    interpretQuery
  , interpretAnonymousQuery
  , Response(..)
    -- * Preparing queries then running them
  , makeSchema
  , compileQuery
  , executeQuery
  , QueryError
  , Schema
  , VariableValues
  , Value
  ) where

import Protolude

import qualified Data.Aeson as Aeson
import qualified Data.Map as Map
import qualified Data.HashMap.Strict as HashMap
import Data.Attoparsec.Text (parseOnly, endOfInput)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import GraphQL.API (HasObjectDefinition(..), SchemaError(..))
import GraphQL.Internal.Execution
  ( VariableValues
  , ExecutionError
  , substituteVariables
  , getDocumentVariableDefinitions
  )
import qualified GraphQL.Internal.Execution as Execution
import qualified GraphQL.Internal.Syntax.AST as AST
import qualified GraphQL.Internal.Syntax.Parser as Parser
import GraphQL.Internal.Validation
  ( QueryDocument
  , SelectionSetByType
  , ValidationErrors
  , validate
  , getSelectionSet
  , getVariableValuesFromJSON
  , VariableValue
  , VariableDefinitions
  )
import GraphQL.Internal.Request 
  ( PostRequest (PostRequest)
  , RawPostRequest (RawPostRequest)
  , PostRequestError (..)
  )
import GraphQL.Internal.Output
  ( GraphQLError(..)
  , Response(..)
  , singleError
  )
import GraphQL.Internal.Schema (Schema)
import qualified GraphQL.Internal.Schema as Schema
import GraphQL.Resolver (HasResolver(..), Result(..))
import GraphQL.Value (Name, Value, pattern ValueObject, makeName)

-- | Errors that can happen while processing a query document.
data QueryError
  -- | Failed to parse.
  = ParseError Text
  -- | Parsed, but failed validation.
  --
  -- See <https://facebook.github.io/graphql/#sec-Validation> for more
  -- details.
  | ValidationError ValidationErrors
  -- | Validated, but failed during execution.
  | ExecutionError ExecutionError
  -- | Error in the schema.
  | SchemaError SchemaError
  -- | Got a value that wasn't an object.
  | NonObjectResult Value
  deriving (Eq, Show)

instance GraphQLError QueryError where
  formatError (ParseError e) =
    "Couldn't parse query document: " <> e
  formatError (ValidationError es) =
    "Validation errors:\n" <> mconcat ["  " <> formatError e <> "\n" | e <- NonEmpty.toList es]
  formatError (ExecutionError e) =
    "Execution error: " <> show e
  formatError (SchemaError e) =
    "Schema error: " <> formatError e
  formatError (NonObjectResult v) =
    "Query returned a value that is not an object: " <> show v

-- | Execute a GraphQL query.
executeQuery
  :: forall api m. (HasResolver m api, Applicative m, HasObjectDefinition api)
  => Handler m api -- ^ Handler for the query. This links the query to the code you've written to handle it.
  -> QueryDocument VariableValue  -- ^ A validated query document. Build one with 'compileQuery'.
  -> Maybe Name -- ^ An optional name. If 'Nothing', then executes the only operation in the query. If @Just "something"@, executes the query named @"something".
  -> VariableValues -- ^ Values for variables defined in the query document. A map of 'Variable' to 'Value'.
  -> m Response -- ^ The outcome of running the query.
executeQuery handler document name variables =
  case getOperation document name variables of
    Left e -> pure (ExecutionFailure (singleError e))
    Right operation -> toResult <$> resolve @m @api handler (Just operation)
  where
    toResult (Result errors result) =
      case result of
        -- TODO: Prevent this at compile time. Particularly frustrating since
        -- we *know* that api has an object definition.
        ValueObject object ->
          case NonEmpty.nonEmpty errors of
            Nothing -> Success object
            Just errs -> PartialSuccess object (map toError errs)
        v -> ExecutionFailure (singleError (NonObjectResult v))

-- | Create a GraphQL schema.
makeSchema :: forall api. HasObjectDefinition api => Either QueryError Schema
makeSchema = first SchemaError (Schema.makeSchema <$> getDefinition @api)

-- | Interpet a GraphQL query.
--
-- Compiles then executes a GraphQL query.
interpretQuery
  :: forall api m. (Applicative m, HasResolver m api, HasObjectDefinition api)
  => Handler m api -- ^ Handler for the query. This links the query to the code you've written to handle it.
  -> Text -- ^ The text of a query document. Will be parsed and then executed.
  -> Maybe Name -- ^ An optional name for the operation within document to run. If 'Nothing', execute the only operation in the document. If @Just "something"@, execute the query or mutation named @"something"@.
  -> VariableValues -- ^ Values for variables defined in the query document. A map of 'Variable' to 'Value'.
  -> m Response -- ^ The outcome of running the query.
interpretQuery handler query name variables =
  case makeSchema @api >>= flip compileQuery query of
    Left err -> pure (PreExecutionFailure (toError err :| []))
    Right document -> executeQuery @api @m handler document name variables

-- | Interpret a GraphQL query, given a packaged request.
interpretPostRequest
  :: forall api m
   . (Applicative m, HasResolver m api, HasObjectDefinition api)
  => Handler m api -- ^ Handler for the query. This links the query to the code you've written to handle it.
  -> PostRequest -- ^ The query and its input values.
  -> m Response -- ^ The outcome of running the query.
interpretRequest handler (PostRequest query name variables) =
  executeQuery @api @m handler query name variables

-- | Interpret an anonymous GraphQL query.
--
-- Anonymous queries have no name and take no variables.
interpretAnonymousQuery
  :: forall api m. (Applicative m, HasResolver m api, HasObjectDefinition api)
  => Handler m api -- ^ Handler for the anonymous query.
  -> Text -- ^ The text of the anonymous query. Should defined only a single, unnamed query operation.
  -> m Response -- ^ The result of running the query.
interpretAnonymousQuery handler query = interpretQuery @api @m handler query Nothing mempty

-- | Turn some text into a valid query document.
compileQuery :: Schema -> Text -> Either QueryError (QueryDocument VariableValue)
compileQuery schema query = do
  parsed <- first ParseError (parseQuery query)
  first ValidationError (validate schema parsed)

-- | Parse a query document.
parseQuery :: Text -> Either Text AST.QueryDocument
parseQuery query = first toS (parseOnly (Parser.queryDocument <* endOfInput) query)

-- | Get an operation from a query document ready to be processed.
getOperation :: QueryDocument VariableValue -> Maybe Name -> VariableValues -> Either QueryError (SelectionSetByType Value)
getOperation document name vars = first ExecutionError $ do
  op <- Execution.getOperation document name
  resolved <- substituteVariables op vars
  pure (getSelectionSet resolved)

-- | Infer variables from a json object with definition context
-- (k -> v -> a -> a) -> a -> HashMap k v -> a 
getVariablesValues
  :: VariableDefinitions
  -> Aeson.Object
  -> Either QueryError VariableValues
getVariablesValues definitions variables = case getVariableValuesFromJSON definitions variables of
  Left validationErrors -> Left (ValidationError validationErrors)
  Right variableValues -> Right variableValues

-- compilePostRequest :: Aeson.Object ->  
-- | Get a request object from a JSON AST and a graphQL 'Schema'.
-- |
-- getRequest :: Schema -> Aeson.Object -> Either QueryError PostRequest
-- getRequest schema value = Left (JSONRequestError (JSONRequestShapeError ""))
--   where 
    -- variableDefinitions = case queryDocument of 
    --   Left e -> Left e
    --   Right (queryDoc) -> case getDocumentVariableDefinitions queryDoc operationName of
    --     Left executionError -> Left (ExecutionError executionError)
    -- operationName = case makeName operationTextName of
    --   Left e -> Left NoQueryDocumentReceived
    --   Just (Aeson.String queryDocument) -> compileQuery schema queryDocument
    
    -- variableValues = case variables of
    --   Nothing -> Right Map.empty
    --   Just _ -> Right Map.empty
      -- Just (Aeson.Object jsonValue) -> getVariablesValues schema jsonValue 
    -- variableDefinitionsOrError = case operationName of
    --   Left e -> Left e 
    --   Right operationName -> Right (getDocumentVariableDefinitions schema jsonValue)
    -- variableDefinitions = getDocumentVariableDefinitions <$> compiledQuery 
    -- variableDefinitions = getDocumentVariableDefinitions <$> query operationName
    -- testQuery = either (<*>) (compileQuery schema) query
    -- query = case HashMap.lookup "query" value of 
    --   Nothing -> Left $ shouldBeAStringError "query"
    --   Just value -> Left $ shouldBeAStringError "query"
    --   Just (Aeson.String rawQueryDocument) -> Right rawQueryDocument 
    --   Just (Aeson.String rawQueryDocument) -> case compileQuery schema rawQueryDocument of 
    --     Left qe -> Left qe
    --     Right query -> Right query
    -- operationName = case HashMap.lookup "operationName" value of 
    --   Nothing -> Right Nothing
    --   Just (Aeson.String opName) -> case makeName opName of
    --     Left _ -> Left (JSONRequestError $ JSONNameError $ "The operationName " <> opName <> " is not a valid graphql name.")
    --     Right name -> Right (Just name)
    --   Just value -> Left $ shouldBeAStringError "operationName" 
    -- variables = HashMap.lookup "variables" value
    -- shouldBeAStringError fieldName = JSONRequestError 
    --   (JSONRequestShapeError $ "The query " <> fieldName <> " in request should be a string field.")

