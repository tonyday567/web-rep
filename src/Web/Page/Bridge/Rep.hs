{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs         #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Web.Page.Bridge.Rep
  ( RepF(..)
  , Rep(..)
  , SharedRepF(..)
  , SharedRep(..)
  , makeRep
  , slider_
  , sliderR_
  , stepslider_
  , stepsliderR_
  , dropdown_
  , color_
  , textbox_
  , checkbox_
  , maybeRep
  , cardifySharedRep
  , consumeSharedBridge
  ) where

import Control.Applicative (liftA2)
import Control.Category (id)
import Control.Lens
import Data.Attoparsec.Text
import Data.Biapplicative (Biapplicative(..))
import Data.Bifunctor (Bifunctor(..))
import Data.HashMap.Strict
import Data.Text (pack, unpack, Text)
import Lucid
import Protolude hiding ((<<*>>), Rep)
import Web.Page.Bootstrap
import Web.Page.Bridge
import qualified Box
import Data.Aeson (Value)
import Web.Page.Html
import Web.Page.Html.Input
import Network.JavaScript
import Codec.Picture.Types (PixelRGB8(..))
import Numeric

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

data SomeSharedRep m where
  SomeSharedRep :: (a -> Html ()) -> SharedRep m a -> SomeSharedRep m

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

slider_ :: (Monad m, ToHtml a, Integral a, Show a) => Text -> a -> a -> a -> SharedRep m a
slider_ label l u = makeRep decimal show $ \name v ->
  (toHtml $ bridgeify $ bootify $ Input v Slider (Just label) Nothing name [("min", pack $ show l), ("max", pack $ show u)])

stepslider_ :: (Monad m, ToHtml a, Integral a, Show a) => Text -> a -> a -> a -> a -> SharedRep m a
stepslider_ label l u s = makeRep decimal show $ \name v ->
  (toHtml $ bridgeify $ bootify $ Input v Slider (Just label) Nothing name [("min", pack $ show l), ("max", pack $ show u), ("step", pack $ show s)])

sliderR_ :: (Monad m) => Text -> Double -> Double -> Double -> SharedRep m Double
sliderR_ label l u = makeRep double show $ \name v ->
  (toHtml $ bridgeify $ bootify $ Input v Slider (Just label) Nothing name [("min", pack $ show l), ("max", pack $ show u)])

stepsliderR_ :: (Monad m) => Text -> Double -> Double -> Double -> Double -> SharedRep m Double
stepsliderR_ label l u s = makeRep double show $ \name v ->
  (toHtml $ bridgeify $ bootify $ Input v Slider (Just label) Nothing name [("min", pack $ show l), ("max", pack $ show u), ("step", pack $ show s)])

textbox_ :: (Monad m) => Text -> Text -> SharedRep m Text
textbox_ label = makeRep takeText id $ \name v ->
  (toHtml $ bridgeify $ bootify $ Input v TextBox (Just label) Nothing name [])

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
color_ label = makeRep fromHex toHex $ \name v ->
  (toHtml $ bridgeify $ bootify $ Input v ColorPicker (Just label) Nothing name [])

dropdown_ :: (Monad m, ToHtml a) =>
  Parser a -> (a -> Text) -> Text -> [Text] -> a -> SharedRep m a
dropdown_ p pr label opts = makeRep p pr $ \name v ->
  (toHtml $ bridgeify $ bootify $
   Input v (Dropdown' opts (Just (pr v))) (Just label) Nothing name [])

checkbox_ :: (Monad m) => Text -> Bool -> SharedRep m Bool
checkbox_ label = makeRep ((=="true") <$> takeText) (bool "false" "true") $ \name v ->
  (toHtml $ bridgeify $ bootify $ Input v (Checkbox v) (Just label) Nothing name [])

wrap :: (Html () -> Html ()) -> Rep a -> Rep a
wrap w (Rep h f) = Rep (w h) f

wrapSharedRep :: (Monad m) => (Html () -> Html ()) -> SharedRep m a -> SharedRep m a
wrapSharedRep w s = SharedRep $ fmap (wrap w) (unrep s)

cardifySharedRep :: (Monad m) => [(Text,Text)] -> Html () -> Maybe Text -> SharedRep m a -> SharedRep m a
cardifySharedRep atts h mt s = SharedRep $ fmap (wrap card) (unrep s) where
  card = cardify atts h mt

checkboxShowJs :: (Monad m) => Text -> Text -> Bool -> SharedRep m Bool
checkboxShowJs label cl = makeRep ((=="true") <$> takeText) (bool "false" "true") $
  \name v ->
      (toHtml $
      showJsInput cl name $
      bridgeify $
      bootify $
      Input v (Checkbox v) (Just label) Nothing name [])

maybeRep :: (Monad m) => Text -> Text -> Bool -> SharedRep m a -> SharedRep m (Maybe a)
maybeRep label cl start sa = SharedRep $ do
  (Rep checkHtml checkFa) <- unrep $ checkboxShowJs label cl start
  (Rep bodyHtml bodyFa) <- unrep sa
  pure $ Rep
    (cardify [] checkHtml Nothing
     (with div_
       [class_ cl, style_
                   ("display:" <> bool "none" "block" start)]
       bodyHtml))
    (\hm -> either Left (bool (Right Nothing) (Just <$> bodyFa hm)) (checkFa hm))

consumeSharedBridge :: Show b => (Engine -> b -> IO a) -> HashMap Text Text -> (Either Text (HashMap Text Text) -> b) -> Event Value -> Engine -> IO (HashMap Text Text)
consumeSharedBridge cc hm fa ev e =
  elementConsume hm (\(Element k v) s -> insert k v s)
  (contramap fa <$>
  ( (Box.liftC <$> Box.showStdout) <>
    pure (Box.Committer (\v -> cc e v >> pure True))
  )) ev e
