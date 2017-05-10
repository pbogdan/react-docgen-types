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

import           Control.Lens hiding ((&))
import           Control.Monad
import           Data.Aeson
import           Data.Aeson.Lens
import           Data.Aeson.Types
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text

data PropType
  = TyArray
  | TyBool
  | TyFunc
  | TyNumber
  | TyObject
  | TyString
  | TySymbol Text
  | TyNode
  | TyElement
  | TyInstanceOf Text
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
      value <- parseJSON =<< (o .: "value") :: Parser Value
      let vals =
            value ^.. _Array . traverse . _Object . ix "value" . _String <&>
            Text.init .
            Text.tail
      case vals of
        [] -> fail "enum value doesn't list any alternatives."
        _ -> return . TyEnum $ vals
    String "union" -> do
      value <- parseJSON =<< (o .: "value" <|> o .: "elements") :: Parser Value
      ms <- sequenceA $ value ^.. _Array . traverse . _Value <&> parsePropType
      case ms of
        [] -> fail "union value with no members"
        _ -> return . TyUnion $ ms
    String "arrayOf" -> do
      typeValue <- parsePropType =<< o .: "value"
      return . TyArrayOf $ typeValue
    String "objectOf" -> do
      typeValue <- parsePropType =<< o .: "value"
      return . TyObjectOf $ typeValue
    String "shape" -> do
      typeValue <- parseJSON =<< o .: "value" :: Parser Value
      props <-
        sequenceA . mconcat $ typeValue ^.. _Object . traverse . _Value .
        _Object <&>
        HashMap.map parsePropType
      case HashMap.toList props of
        [] -> fail "shape with no properties"
        xs -> return . TyShape $ xs
    String "custom" -> pure TyCustom
    String "Function" -> pure TyFunction
    String "function" -> pure TyFunction
    String "any" -> pure TyAny
    _ -> fail $ "parsePropType: encountered unknown value: " <> show o
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
