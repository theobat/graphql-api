
module GraphQL.Request
( VariableError
-- , scalarJSONToAST
) where

import Protolude

import qualified Data.Aeson as Aeson
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
import qualified GraphQL.Internal.Syntax.AST as AST
import qualified GraphQL.Internal.Syntax.Parser as Parser
import GraphQL.Internal.Validation
  ( QueryDocument
  , SelectionSetByType
  , ValidationErrors
  , validate
  , getSelectionSet
  , VariableValue
  )
import GraphQL.Internal.Output
  ( GraphQLError(..)
  , Response(..)
  , singleError
  )
import GraphQL.Internal.Schema (Schema, Builtin(..), GType)
import qualified GraphQL.Internal.Schema as Schema
import GraphQL.Resolver (HasResolver(..), Result(..))
import GraphQL.Value (Name, Value)
import Data.Scientific as Scientific

data VariableError
  -- | A variable value is defined in the JSON map but not in the query
  = UnexpectedVariableValue AST.Variable
  -- | A variable value is undefined in the JSON map but is expected in the query (as NonNull, otherwise, it's considered valid)
  | MissingVariableValue AST.Variable
 -- | The Variable is defined its 'Data.Aeson.Value' does not conform to the expected 'GType'
  | JSONTypeMismatch Aeson.Value GType AST.Variable
  -- | Numeric Conversion Error (From JSON Scientific number reprensentation to Native haskell types)
  | NumericConversionError Aeson.Value GType AST.Variable
  deriving (Eq, Show)
 
 -- | Convert a constant Aeson (JSON) scalar value to an 'AST.Value' scalar value
 -- scalarJSONToAST :: Variable -> GType -> Aeson.Value -> Either VariableError AST.Value
 -- scalarJSONToAST v (BuiltinType GFloat) (Aeson.Number x) = Right (AST.ValueFloat (Scientific.toRealFloat x))
 -- scalarJSONToAST v (BuiltinType GFloat) x = Left (JSONTypeMismatch x (BuiltinType GFloat) v)
 -- scalarJSONToAST v (BuiltinType GInt) (Aeson.Number x) = maybeToLeft (Scientific.toBoundedInteger x) (NumericConversionError v)
-- | Convert a constant Aeson (JSON) scalar value to an 'AST.Value' scalar value


numberJSONToAST :: AST.Variable -> Builtin -> Scientific -> Either VariableError AST.Value
numberJSONToAST v gtype x
  | Scientific.isFloating x && gtype == GFloat = Right (AST.ValueFloat (Scientific.toRealFloat x)) 
  | Scientific.isInteger x && gtype == GInt = Right (Just (Scientific.toBoundedInteger x))
  | otherwise = Left (JSONTypeMismatch (Aeson.Number x) (Schema.BuiltinType gtype) v) 
  -- | Scientific.isFloating x && not (gtype == GFloat) = Left (JSONTypeMismatch (Aeson.Number x) (Schema.BuiltinType gtype) v) 

  -- | Scientific.isInteger x =  


-- scalarJSONToAST :: AST.Variable -> Builtin -> Aeson.Value -> Either VariableError AST.Value
-- scalarJSONToAST v GFloat (Aeson.Number x) 
--     | weight / height ^ 2 <= 18.5 = "You're underweight, you emo, you!"  
--     | weight / height ^ 2 <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"  
--     | weight / height ^ 2 <= 30.0 = "You're fat! Lose some weight, fatty!"  
--     | otherwise                 
-- | bmi <= 18.5 = "You're underweight, you emo, you!"  
-- | bmi <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"  
-- | bmi <= 30.0 = "You're fat! Lose some weight, fatty!"  
-- | otherwise   = "You're a whale, congratulations!"  
-- scalarJSONToAST v GFloat (Aeson.Number x) = Right (AST.ValueFloat (Scientific.toRealFloat x))
-- scalarJSONToAST v GFloat x = Left (JSONTypeMismatch x (Schema.BuiltinType GFloat) v)
-- scalarJSONToAST v GInt (Aeson.Number x) = maybeToEither (NumericConversionError v) (Scientific.toBoundedInteger x)
