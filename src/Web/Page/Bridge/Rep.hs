{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Web.Page.Bridge.Rep
  ( RepF(..)
  , Rep
  , SharedRepF(..)
  , SharedRep
  , makeRep
  , sliderI_
  , slider_
  , dropdown_
  , color_
  , fromHex
  , toHex
  , PixelRGB8(..)
  , textbox_
  , checkbox_
  , toggle_
  , button_
  , maybeRep
  , updateMap
  ) where

import Codec.Picture.Types (PixelRGB8(..))
import Control.Applicative (liftA2)
import Control.Category (id)
import Control.Lens
import Data.Aeson (Value)
import Data.Attoparsec.Text
import Data.Biapplicative
import Data.Bifunctor (Bifunctor(..))
import Data.HashMap.Strict
import Data.Text (pack, Text)
import Lucid hiding (button_)
import Network.JavaScript hiding (delete)
import Numeric
import Protolude hiding ((<<*>>), Rep)
import Web.Page.Bootstrap
import Web.Page.Bridge
import Web.Page.Html
import Web.Page.Html.Input
import qualified Box

data RepF r a = Rep
  { rep :: r
  , make :: HashMap Text Text -> Either Text a
  } deriving (Functor)

type Rep a = RepF (Html ()) a 

instance Bifunctor RepF where
  bimap f g (Rep r a) = Rep (f r) (fmap g . a)

instance Biapplicative RepF where
  bipure r a = Rep r (Right . const a)
  (Rep fr fa) <<*>> (Rep r a) = Rep (fr r) (\m -> fa m <*> a m)

instance (Monoid r) => Applicative (RepF r) where
  pure = bipure mempty
  Rep fh fm <*> Rep ah am =
    Rep (fh <> ah) (\m -> fm m <*> am m)

newtype SharedRepF m r a = SharedRep
  { unrep :: StateT (Int, HashMap Text Text) m (RepF r a)
  } deriving Functor

type SharedRep m a = SharedRepF m (Html ()) a 

instance (Functor m) => Bifunctor (SharedRepF m) where
  bimap f g (SharedRep s) = SharedRep $ fmap (bimap f g) s

instance (Monad m) => Biapplicative (SharedRepF m) where
  bipure r a = SharedRep $ pure $ bipure r a
  (SharedRep f) <<*>> (SharedRep a) = SharedRep $ liftA2 (<<*>>) f a

instance (Monad m, Monoid r) => Applicative (SharedRepF m r) where
  pure = bipure mempty
  SharedRep f <*> SharedRep a = SharedRep $ liftA2 (<*>) f a

makeRep ::
  (Monad m) =>
  Parser a ->
  (a -> Text) ->
  (Text -> a -> Html ()) ->
  a ->
  SharedRep m a
makeRep p pr f a =
  SharedRep $ do
    name <- zoom _1 genName
    zoom _2 (modify (insert name (pr a)))
    pure $
      Rep
      (f name a)
      (\s ->
        join $
        maybe (Left "lookup failed") Right $
        either (Left . pack) Right . parseOnly p <$> lookup name s)

repInput :: (ToHtml a, Monad m) => Parser a -> (a -> Text) -> Input a -> a -> SharedRep m a
repInput p pr i a =
  SharedRep $ do
    name <- zoom _1 genName
    zoom _2 (modify (insert name (pr a)))
    pure $
      Rep
      (toHtml $ bridgeify $ bootify $ set #val a $ set #id' name i)
      (\s ->
        join $
        maybe (Left "lookup failed") Right $
        either (Left . pack) Right . parseOnly p <$> lookup name s)

{-
-- | does not put a value into the HashMap on instantiation, and consumes the value when found in the HashMap
repMessage :: (ToHtml a, Monad m) => Parser a -> (a -> Text) -> Input a -> a -> SharedRep m a
repMessage p pr i a =
  SharedRep $ do
    name <- zoom _1 genName
    -- zoom _2 (modify (insert name (pr a)))
    Rep <$> 
      pure (toHtml $ bridgeify $ bootify $ set #val a $ set #id' name i) <*>
      pure (\s -> do
        let res = join $
              maybe (Left "lookup failed") Right $
              either (Left . pack) Right . parseOnly p <$>
              lookup name s
        put (delete name s)
        res)
-}

slider_ :: (Monad m) => Text -> Double -> Double -> Double -> Double -> SharedRep m Double
slider_ label l u s v = repInput double show
  (Input v Slider (Just label) Nothing mempty [("min", pack $ show l), ("max", pack $ show u), ("step", pack $ show s)]) v

sliderI_ :: (Monad m, ToHtml a, Integral a, Show a) => Text -> a -> a -> a -> a -> SharedRep m a
sliderI_ label l u s v = repInput decimal show 
  (Input v Slider (Just label) Nothing mempty [("min", pack $ show l), ("max", pack $ show u), ("step", pack $ show s)]) v

textbox_ :: (Monad m) => Text -> Text -> SharedRep m Text
textbox_ label v = repInput takeText id
  (Input v TextBox (Just label) Nothing mempty []) v

fromHex :: Parser PixelRGB8
fromHex =
  (\((r,g),b) ->
       PixelRGB8 (fromIntegral r) (fromIntegral g) (fromIntegral b)) .
    (\(f,b) -> (f `divMod` (256 :: Int), b)) .
    (`divMod` 256) <$>
    (string "#" *> hexadecimal)

toHex :: PixelRGB8 -> Text
toHex (PixelRGB8 r g b) = pack $ "#" <> showHex r (showHex g $ showHex b "")

instance ToHtml PixelRGB8 where
  toHtml = toHtml . toHex
  toHtmlRaw = toHtmlRaw . toHex

color_ :: (Monad m) => Text -> PixelRGB8 -> SharedRep m PixelRGB8
color_ label v = repInput fromHex toHex
  (Input v ColorPicker (Just label) Nothing mempty []) v

dropdown_ :: (Monad m, ToHtml a) =>
  Parser a -> (a -> Text) -> Text -> [Text] -> a -> SharedRep m a
dropdown_ p pr label opts v = repInput p pr 
  (Input v (Dropdown opts (Just (pr v))) (Just label) Nothing mempty []) v

checkbox_ :: (Monad m) => Text -> Bool -> SharedRep m Bool
checkbox_ label v = repInput ((=="true") <$> takeText) (bool "false" "true")
  (Input v (Checkbox v) (Just label) Nothing mempty []) v

toggle_ :: (Monad m) => Text -> Bool -> SharedRep m Bool
toggle_ label v = repInput ((=="true") <$> takeText) (bool "false" "true")
  (Input v (Toggle v label) Nothing Nothing mempty []) v

instance ToHtml () where
  toHtml = const "()"
  toHtmlRaw = const "()"

button_ :: (Monad m) => Text -> SharedRep m ()
button_ label = repInput (pure ()) (const "")
  (Input () (Button label) Nothing Nothing mempty []) ()

checkboxShowJs :: (Monad m) => Text -> Text -> Bool -> SharedRep m Bool
checkboxShowJs label cl = makeRep ((=="true") <$> takeText) (bool "false" "true") $
  \name v ->
      (toHtml $
      showJsInput cl name $
      bridgeify $
      bootify $
      Input v (Checkbox v) (Just label) Nothing name [])

maybeRep :: (Monad m) => Text -> Text -> Bool -> SharedRep m a -> SharedRep m (Maybe a)
maybeRep label cl st sa = bimap hmap mmap (checkboxShowJs label cl st) <<*>> sa
  where
    hmap a b =
      cardify [] a Nothing
       (with div_
        [class_ cl, style_
                   ("display:" <> bool "none" "block" st)]
        b)
    mmap a b = bool Nothing (Just b) a

updateMap :: Show b => (Engine -> b -> IO a) -> HashMap Text Text -> (Either Text (HashMap Text Text) -> b) -> Event Value -> Engine -> IO (HashMap Text Text)
updateMap cc hm fa ev e =
  elementConsume hm (\(Element k v) s -> insert k v s)
  (contramap fa <$>
  ( (Box.liftC <$> Box.showStdout) <>
    pure (Box.Committer (\v -> cc e v >> pure True))
  )) ev e
