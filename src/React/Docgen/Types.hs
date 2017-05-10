module React.Docgen.Types
  ( PropType(..)
  , Components(..)
  , Component(..)
  , Prop(..)
  , DefaultValue(..)
  , decode
  , decode'
  , eitherDecode
  , eitherDecode'
  , eitherDecodeStrict
  , eitherDecodeStrict'
  )
where

import           Protolude

import           Control.Monad
import           Data.Aeson
import           Data.Aeson.Types
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text
import qualified Data.Vector as Vec

data PropType
  = TyArray
  | TyBool
  | TyFunc
  | TyNumber
  | TyObject
  | TyString
  | TySymbol Text -- literal
  | TyNode
  | TyElement
  | TyInstanceOf Text -- Number, Message
  | TyEnum [Text]
  | TyUnion [PropType]
  | TyArrayOf PropType
  | TyObjectOf PropType
  | TyShape [(Text, PropType)]
  | TyFunction
  | TyCustom
  | TyNullable PropType
  | TyAny
  deriving (Eq, Show)

parsePropType :: Value -> Parser PropType
parsePropType (Object o) = do
  typeName <- o .: "name"
  case typeName of
    String "array" -> pure TyArray
    String "bool" -> pure TyBool
    String "boolean" -> pure TyBool
    String "HiddenProps" -> pure TyBool
    String "func" -> pure TyFunc
    String "number" -> pure TyNumber
    String "object" -> pure TyObject
    String "string" -> pure TyString
    String "literal" -> do
      typeValue <- o .: "value"
      return (TySymbol typeValue)
    String "node" -> pure TyNode
    String "element" -> pure TyElement
    String "Element" -> pure TyElement
    String "instanceOf" -> do
      typeValue <- o .: "value"
      return . TyInstanceOf $ typeValue
    String "enum" -> do
      values <- parseJSON =<< (o .: "value")
      vals <-
        case values of
          Array xs ->
            Vec.forM xs $ \x ->
              case x of
                Object oo -> do
                  value <- oo .: "value"
                  case value of
                    String s -> pure . Text.init . Text.tail $ s
                    _ -> fail "meh1"
                _ -> fail "meh2"
          _ -> fail "meh3"
      return . TyEnum . Vec.toList $ vals
    String "union" -> do
      values <- parseJSON =<< (o .: "value" <|> o .: "elements")
      vals <-
        case values of
          Array xs ->
            Vec.forM xs $ \x ->
              case x of
                js@(Object _) -> parsePropType js
                _ -> fail "meh4"
          _ -> fail "meh5"
      return . TyUnion . Vec.toList $ vals
    String "arrayOf" -> do
      typeValue <- parsePropType =<< o .: "value"
      return . TyArrayOf $ typeValue
    String "objectOf" -> do
      typeValue <- parsePropType =<< o .: "value"
      return . TyObjectOf $ typeValue
    String "shape" -> do
      typeValue <- parseJSON =<< o .: "value"
      case typeValue of
        Object oo -> do
          subProps <- sequenceA $ HashMap.map parsePropType oo
          return . TyShape . HashMap.toList $ subProps
        _ -> fail "meh6"
    String "custom" -> pure TyCustom
    String "Function" -> pure TyFunction
    String "function" -> pure TyFunction
    String "any" -> pure TyAny
    _ -> fail $ "meh7: " <> show o
parsePropType (Array _) = mzero
parsePropType (String _) = mzero
parsePropType (Number _) = mzero
parsePropType (Bool _) = mzero
parsePropType Null = mzero

newtype Components =
  Components (HashMap Text Component)
  deriving (Eq, Show)

instance FromJSON Components where
  parseJSON (Object o) = do
    comps <- sequenceA . HashMap.map parseJSON $ o
    pure . Components $ comps
  parseJSON _ = mzero

data Component = Component
  { componentDescription :: Text
  -- , componentMethods :: [Text]
  , componentProps :: HashMap Text Prop
  } deriving (Eq, Show)

instance FromJSON Component where
  parseJSON (Object o) =
    Component <$> o .: "description" <*>
    (sequenceA . HashMap.map parseJSON =<< o .: "props")
  parseJSON _ = mzero

data Prop = Prop
  { propType :: PropType
  , propDescription :: Text
  , propDefaultValue :: Maybe DefaultValue
  } deriving (Eq, Show)

instance FromJSON Prop where
  parseJSON (Object o) = do
    required <- o .: "required" <|> pure False
    propType_ <-
      do ret <- parsePropType =<< (o .: "type" <|> o .: "flowType")
         if required
           then pure ret
           else pure . TyNullable $ ret
    desc <- o .: "description"
    defValue <- o .:? "defaultValue"
    return $ Prop propType_ desc defValue
  parseJSON _ = mzero

data DefaultValue = DefaultValue
  { defaultValueValue :: Value
  , defaultValueComputed :: Bool
  } deriving (Eq, Show)


instance FromJSON DefaultValue where
  parseJSON (Object o) = DefaultValue <$> o .: "value" <*> o .: "computed"
  parseJSON _ = mzero
