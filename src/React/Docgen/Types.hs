{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}

module React.Docgen.Types
  ( Component(..)
  , Components(..)
  , DefaultValue(..)
  , Prop(..)
  , PropType(..)
  , TypeSimple(..)
  , TypeComplex(..)
  , (:|:)(..)
  , isAltLeft
  , isAltRight
  , altLefts
  , altRights
  , componentSourceFile
  , componentName
  , componentDescription
  , componentProps
  , defaultValueComputed
  , defaultValueValue
  , propName
  , propDefaultValue
  , propDescription
  , propType
  , decode
  , decode'
  , eitherDecode
  , eitherDecode'
  , eitherDecodeStrict
  , eitherDecodeStrict'
  )
where

import           Protolude hiding (sourceFile)

import           Control.Lens hiding ((&))
import           Control.Monad
import           Data.Aeson
import           Data.Aeson.Lens
import           Data.Aeson.Types
import qualified Data.HashMap.Strict as HashMap
import           System.FilePath.Lens

data a :|: b
  = AltLeft a
  | AltRight b
  deriving (Show, Eq, Ord, Functor)
infixr 5 :|:

instance Bifunctor (:|:) where
  bimap f _ (AltLeft a) = AltLeft (f a)
  bimap _ g (AltRight b) = AltRight (g b)

instance Applicative ((:|:) a) where
  pure = AltRight
  AltLeft e <*> _ = AltLeft e
  AltRight f <*> r = fmap f r

instance Monad ((:|:) a) where
  AltLeft l >>= _ = AltLeft l
  AltRight r >>= k = k r

instance Foldable ((:|:) a) where
  foldMap _ (AltLeft _) = mempty
  foldMap f (AltRight y) = f y
  foldr _ z (AltLeft _) = z
  foldr f z (AltRight y) = f y z
  length (AltLeft _) = 0
  length (AltRight _) = 1
  null = isAltLeft

instance Traversable ((:|:) a) where
  traverse _ (AltLeft x) = pure (AltLeft x)
  traverse f (AltRight y) = AltRight <$> f y

isAltLeft :: a :|: b -> Bool
isAltLeft (AltLeft _) = True
isAltLeft (AltRight _) = False

isAltRight :: a :|: b -> Bool
isAltRight = not . isAltLeft

altLefts :: [a :|: b] -> [a]
altLefts ls = [x | AltLeft x <- ls]

altRights :: [a :|: b] -> [b]
altRights ls = [x | AltRight x <- ls]

data TypeSimple =
  TypeSimple
  deriving (Eq, Show)

data TypeComplex =
  TypeComplex
  deriving (Eq, Show)

data PropType a where
        TyArray :: PropType TypeComplex
        TyBool :: PropType TypeSimple
        TyFunc :: PropType TypeComplex
        TyNumber :: PropType TypeSimple
        TyObject :: PropType TypeComplex
        TyString :: PropType TypeSimple
        TySymbol :: Text -> PropType TypeSimple
        TyNode :: PropType TypeComplex
        TyElement :: PropType TypeComplex
        TyInstanceOf :: Text -> PropType TypeComplex
        TyEnum :: [Text] -> PropType TypeSimple
        TyUnion ::
          [PropType TypeSimple :|: PropType TypeComplex] -> PropType a
        TyArrayOf :: PropType a -> PropType a
        TyObjectOf :: PropType a -> PropType a
        TyShape ::
          [(Text, PropType TypeSimple :|: PropType TypeComplex)] ->
            PropType a
        TyFunction :: PropType TypeComplex
        TyCustom :: PropType TypeComplex
        TyNullable :: PropType a -> PropType a
        TyAny :: PropType TypeComplex

deriving instance Eq a => Eq (PropType a)
deriving instance Show a => Show (PropType a)

isPropTypeSimple :: PropType TypeSimple :|: PropType TypeComplex -> Bool
isPropTypeSimple (AltLeft _) = True
isPropTypeSimple (AltRight _) = False

instance FromJSON (PropType TypeSimple :|: PropType TypeComplex) where
  parseJSON (Object o) = do
    typeName <- o .: "name" :: Parser Text
    case typeName of
      "array" -> pure . AltRight $ TyArray
      "bool" -> pure . AltLeft $ TyBool
      "boolean" -> pure . AltLeft $ TyBool
      "HiddenProps" -> pure . AltLeft $ TyBool
      "func" -> pure . AltRight $ TyFunc
      "number" -> pure . AltLeft $ TyNumber
      "object" -> pure . AltRight $ TyObject
      "string" -> pure . AltLeft $ TyString
      "literal" -> do
        typeValue <- o .: "value"
        return . AltLeft $ TySymbol typeValue
      "node" -> pure . AltRight $ TyNode
      "element" -> pure . AltRight $ TyElement
      "Element" -> pure . AltRight $ TyElement
      "instanceOf" -> do
        typeValue <- o .: "value"
        return . AltRight $ TyInstanceOf typeValue
      "enum" -> do
        value <- parseJSON =<< o .: "value" :: Parser Value
        let vals =
              value ^.. _Array . traverse . _Object . ix "value" . _String
        case vals of
          [] -> fail "enum value doesn't list any alternatives."
          _ -> return . AltLeft $ TyEnum vals
      "union" -> do
        value <-
          parseJSON =<< (o .: "value" <|> o .: "elements") :: Parser Value
        ms <- sequenceA $ value ^.. _Array . traverse . _Value <&> parseJSON
        if getAll . mconcat . map (All . isPropTypeSimple) $ ms
          then pure . AltLeft . TyUnion $ ms
          else pure . AltRight . TyUnion $ ms
      "arrayOf" -> do
        typeValue <- parseJSON =<< o .: "value"
        case typeValue of
          AltLeft l -> pure . AltLeft $ TyArrayOf l
          AltRight r -> pure . AltRight $ TyArrayOf r
      "objectOf" -> do
        typeValue <- parseJSON =<< o .: "value"
        case typeValue of
          AltLeft l -> pure . AltLeft $ TyArrayOf l
          AltRight r -> pure . AltRight $ TyArrayOf r
      "shape" -> do
        typeValue <- parseJSON =<< o .: "value" :: Parser Value
        props <-
          sequenceA . mconcat $ typeValue ^.. _Object . traverse . _Value .
          _Object <&>
          HashMap.map parseJSON
        if getAll . mconcat . map (All . isPropTypeSimple) $ HashMap.elems props
          then pure . AltLeft . TyShape $ HashMap.toList props
          else pure . AltRight . TyShape $ HashMap.toList props
      "custom" -> pure . AltRight $ TyCustom
      "Function" -> pure . AltRight $ TyFunction
      "function" -> pure . AltRight $ TyFunction
      "any" -> pure . AltRight $ TyAny
      _ -> fail $ "parsePropType: encountered unknown value: " <> show o
  parseJSON (Array _) = mzero
  parseJSON (String _) = mzero
  parseJSON (Number _) = mzero
  parseJSON (Bool _) = mzero
  parseJSON Null = mzero

newtype Components =
  Components [Component]
  deriving (Eq, Show)

instance FromJSON Components where
  parseJSON (Object o) = do
    comps <- sequenceA . HashMap.elems . HashMap.mapWithKey parseComponent $ o
    pure . Components $ comps
  parseJSON _ = mzero

data Component = Component
  { _componentSourceFile :: Text
  , _componentName :: Text
  , _componentDescription :: Text
  , _componentProps :: [Prop TypeSimple :|: Prop TypeComplex]
  } deriving (Eq, Show)

parseComponent :: Text -> Value -> Parser Component
parseComponent sourceFile (Object o) = do
  description <- o .: "description"
  props <-
    sequenceA .
    HashMap.elems .
    HashMap.mapWithKey
      (\k v ->
         (pure . AltLeft =<< parsePropSimple k v) <|>
         (pure . AltRight =<< parsePropComplex k v)) =<<
    (o .: "props")
  return $
    Component
      sourceFile
      (toS . view basename . toS $ sourceFile)
      description
      props
parseComponent _ _ = mzero

data Prop a = Prop
  { _propName :: Text
  , _propType :: PropType a
  , _propDescription :: Text
  , _propDefaultValue :: Maybe DefaultValue
  } deriving (Eq, Show)

parsePropCommon
  :: (FromJSON a, FromJSON b)
  => Object -> Parser (PropType TypeSimple :|: PropType TypeComplex, b, Maybe a)
parsePropCommon o = do
  required <- o .: "required" <|> pure False
  propType_ <-
    do ret <-
         parseJSON =<< (o .: "type" <|> o .: "flowType") :: Parser (PropType TypeSimple :|: PropType TypeComplex)
       case ret of
         AltLeft l ->
           if required
             then pure . AltLeft $ l
             else pure . AltLeft . TyNullable $ l
         AltRight r ->
           if required
             then pure . AltRight $ r
             else pure . AltRight . TyNullable $ r
  desc <- o .: "description"
  defValue <- o .:? "defaultValue"
  return (propType_, desc, defValue)

parsePropSimple :: Text -> Value -> Parser (Prop TypeSimple)
parsePropSimple propName (Object o) = do
  (propType_, desc, defValue) <- parsePropCommon o
  case propType_ of
    AltLeft l -> return $ Prop propName l desc defValue
    AltRight _ -> mzero
parsePropSimple _ _ = mzero

parsePropComplex :: Text -> Value -> Parser (Prop TypeComplex)
parsePropComplex propName (Object o) = do
  (propType_, desc, defValue) <- parsePropCommon o
  case propType_ of
    AltLeft _ -> mzero
    AltRight r -> return $ Prop propName r desc defValue
parsePropComplex _ _ = mzero

data DefaultValue = DefaultValue
  { _defaultValueValue :: Value
  , _defaultValueComputed :: Bool
  } deriving (Eq, Show)

instance FromJSON DefaultValue where
  parseJSON (Object o) = DefaultValue <$> o .: "value" <*> o .: "computed"
  parseJSON _ = mzero

$(makeLenses ''Component)
$(makeLenses ''Prop)
$(makeLenses ''DefaultValue)
