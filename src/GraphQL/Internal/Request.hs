{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Description: How we encode GraphQL responses
module GraphQL.Internal.Request
    (
      RawPostRequest (RawPostRequest)
    )
where
    
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
import GraphQL.Internal.Schema (Schema)
import qualified GraphQL.Internal.Schema as Schema
import GraphQL.Value (Name, Value, pattern ValueObject, makeName)

-- | A request to execute a GraphQL query with some context.
-- See <http://graphql.org/learn/serving-over-http/#post-request>.
-- data PostRequest = PostRequest (QueryDocument VariableValue) (Maybe Name) VariableValues deriving (Eq, Show)

-- | The raw (textual and/or aeson based) version of the 'Request' datatype.
-- See <http://graphql.org/learn/serving-over-http/#post-request>.
data RawPostRequest = RawPostRequest Text (Maybe Text) Aeson.Object deriving (Eq, Show)

-- | Errors that can happen while processing a json 'Request'.
data PostRequestError = RequestBaseNameError Text
  -- | Errors in JSON value's shape.
  | RequestParsingError Text
  -- | Failed JSON values validation.
  -- | RequestValidationError ValidationErrors
  deriving (Eq, Show)

-- A function to extract RawPostRequest from an aeson object.
extractRawPostRequest :: Aeson.Object -> Either PostRequestError RawPostRequest
extractRawPostRequest input = Left (RequestParsingError ("" :: Text))

-- | Infer variables from a json object with definition context
-- (k -> v -> a -> a) -> a -> HashMap k v -> a 
getVariablesValues  :: VariableDefinitions -> Aeson.Object -> Either PostRequestError VariableValues
getVariablesValues definitions variables =
  case getVariableValuesFromJSON definitions variables of
    -- Left  validationErrors -> Left (ValidationErrors validationErrors)
    Right variableValues   -> Right variableValues
