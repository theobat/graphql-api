
module GraphQL.Internal.Request
( RequestError
-- , scalarJSONToAST
) where

import Protolude

import qualified Data.Aeson as Aeson
import Data.Maybe (fromJust)
import Data.Attoparsec.Text (parseOnly, endOfInput)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import GraphQL.API (HasObjectDefinition(..), SchemaError(..))
import GraphQL.Internal.Execution
  ( VariableValues
  , ExecutionError
  , substituteVariables
  )
import qualified GraphQL.Internal.Execution as Execution
import GraphQL.Internal.Syntax.AST (GType(..))
import qualified GraphQL.Internal.Syntax.AST as AST
import qualified GraphQL.Internal.Syntax.Parser as Parser
import GraphQL.Internal.Validation
  ( QueryDocument
  , SelectionSetByType
  , ValidationErrors
  , validate
  , getSelectionSet
  , VariableValue
  , VariableDefinition
  , variableType
  )
import GraphQL.Internal.Output
  ( GraphQLError(..)
  , Response(..)
  , singleError
  )
import GraphQL.Internal.Schema (Schema, AnnotatedType, InputTypeDefinition)
import qualified GraphQL.Internal.Schema as Schema
import GraphQL.Resolver (HasResolver(..), Result(..))
import GraphQL.Internal.Value (Name, Value(..), Value'(ValueScalar'), ConstScalar(..))
import Data.Scientific as Scientific

data RequestError
  -- | A variable value is defined in the JSON map but not in the query
  = UnexpectedVariableValue AST.Variable
  -- | A variable value is undefined in the JSON map but is expected in the query (as NonNull, otherwise, it's considered valid)
  -- | MissingVariableValue AST.Variable
 -- | The Variable is defined its 'Data.Aeson.Value' does not conform to the expected 'GType'
  -- | JSONToSchemaTypeMismatch Aeson.Value GType AST.Variable
  -- | Numeric Conversion Error (From JSON Scientific number reprensentation to Native haskell types)
  -- | NumericConversionError Aeson.Value GType AST.Variable
  -- | InvalidEnum  
  deriving (Eq, Show)
 
data VariableError
  -- | A variable value is defined in the JSON map but not in the query
  = GenericVError
  -- | A variable value is undefined in the JSON map but is expected in the query (as NonNull, otherwise, it's considered valid)
  -- | MissingVariableValue AST.Variable
 -- | The Variable is defined its 'Data.Aeson.Value' does not conform to the expected 'GType'
  -- | JSONToSchemaTypeMismatch Aeson.Value GType AST.Variable
  -- | Numeric Conversion Error (From JSON Scientific number reprensentation to Native haskell types)
  -- | NumericConversionError Aeson.Value GType AST.Variable
  -- | InvalidEnum  
  deriving (Eq, Show)  

  -- data GType = TypeNamed NamedType
  -- | TypeList ListType
  -- | TypeNonNull NonNullType
  -- TypeNamed t
  -- | TypeList (ListType t)
  -- | TypeNonNull 
-- variableJSONToValue :: 

-- | Translate a type definition and a JSON object into a GraphQL Value
-- | 
gTypeValueFromJSON :: AST.GType -> Schema.Schema -> Aeson.Value -> Either VariableError Value
gTypeValueFromJSON (AST.TypeNamed vd) s v = Left GenericVError
gTypeValueFromJSON (AST.TypeList vd) s v = Left GenericVError
gTypeValueFromJSON (AST.TypeNonNull vd) s v = Left GenericVError
-- gTypeValueFromJSON (Schema.InputTypeDefinitionObject td) x = Right (ValueScalar' (ConstInt 12)) -- dumb body for now
-- gTypeValueFromJSON (Schema.InputTypeDefinitionScalar td) x = Right (ValueScalar' (ConstInt 12)) -- dumb body for now
-- gTypeValueFromJSON (Schema.InputTypeDefinitionEnum td) x = Left GenericVError -- dumb body for now
  


-- | Translate a type definition and a JSON object into a GraphQL Value
-- | 
annotatedTypeValueFromJSON :: Schema.AnnotatedType Schema.InputTypeDefinition -> Aeson.Value -> Either VariableError Value
annotatedTypeValueFromJSON (Schema.TypeNamed td) x = Right (ValueScalar' (ConstInt 12)) -- dumb body for now
-- annotatedTypeValueFromJSON (Schema.InputTypeDefinitionScalar td) x = Right (ValueScalar' (ConstInt 12)) -- dumb body for now
-- annotatedTypeValueFromJSON (Schema.InputTypeDefinitionEnum td) x = Left GenericVError -- dumb body for now
  