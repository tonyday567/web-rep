{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs         #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Web.Page.Html.Scooter where

import Protolude hiding ((<<*>>))
import Control.Category (id)
import           Control.Applicative (liftA2)
import           Control.Monad.STM (STM)
import           Data.Aeson (FromJSON (..), Value (), genericParseJSON, Options (..), defaultOptions, camelTo2)
import           GHC.Generics (Generic)
-- import qualified Streaming as S
import           Lucid
import Control.Monad.State
import Data.Attoparsec.Text
import Data.Text (pack, unpack, Text)
import qualified Streaming as S
import qualified Streaming.Prelude as S
import Web.Page.JSB
import Web.Page.Html.Input
import Web.Page.Bootstrap hiding (genName)
import Control.Lens
import Data.HashMap.Strict
import Data.Colour.SRGB (Colour, sRGB24show, sRGB24read)
import Text.InterpolatedString.Perl6
import qualified GHC.Show
import           Data.Bifunctor (Bifunctor(..))
import           Data.Biapplicative (Biapplicative(..))

data ScooterF r a = Scooter
  { rep :: r
  , make :: HashMap Text Text -> Either Text a
  } deriving (Functor)

type Scooter a = ScooterF (Html ()) a 

instance Bifunctor ScooterF where
  bimap f g (Scooter r a) = Scooter (f r) (fmap g . a)

instance Biapplicative ScooterF where
  bipure r a = Scooter r (Right . const a)
  (Scooter fr fa) <<*>> (Scooter r a) = Scooter (fr r) (\m -> fa m <*> a m)

instance (Monoid r) => Applicative (ScooterF r) where
  pure = bipure mempty
  Scooter fh fm <*> Scooter ah am =
    Scooter (fh <> ah) (\m -> fm m <*> am m)

newtype ScootF m r a = Scoot
  { unscoot :: StateT (Int, HashMap Text Text) m (ScooterF r a)
  } deriving Functor

type Scoot m a = ScootF m (Html ()) a 

instance (Functor m) => Bifunctor (ScootF m) where
  bimap f g (Scoot s) = Scoot $ fmap (bimap f g) s

instance (Monad m) => Biapplicative (ScootF m) where
  bipure r a = Scoot $ pure $ bipure r a
  (Scoot f) <<*>> (Scoot a) = Scoot $ liftA2 (<<*>>) f a

instance (Monad m, Monoid r) => Applicative (ScootF m r) where
  pure = bipure mempty
  Scoot f <*> Scoot a = Scoot $ liftA2 (<*>) f a

data SomeScoot m where
  SomeScoot :: (a -> Html ()) -> Scoot m a -> SomeScoot m

genName :: MonadState Int m => m Text
genName = do
  modify (+1)
  show <$> get

mkScooter ::
  (Monad m) =>
  Parser a ->
  (a -> Text) ->
  (Text -> a -> Html ()) ->
  a ->
  Scoot m a
mkScooter p pr f a =
  Scoot $ do
    name <- zoom _1 genName
    zoom _2 (modify (insert name (pr a)))
    pure $
      Scooter
      (f name a)
      (\s ->
        join $
        maybe (Left "lookup failed") Right $
        either (Left . pack) Right . parseOnly p <$> lookup name s)

slider_ :: (Monad m, ToHtml a, Integral a, Show a) => Text -> a -> a -> a -> Scoot m a
slider_ label l u = mkScooter decimal show $ \name v ->
  (toHtml $ jsbify $ bootify $ Input v Slider (Just label) Nothing name [("min", pack $ show l), ("max", pack $ show u)])

stepslider_ :: (Monad m, ToHtml a, Integral a, Show a) => Text -> a -> a -> a -> a -> Scoot m a
stepslider_ label l u s = mkScooter decimal show $ \name v ->
  (toHtml $ jsbify $ bootify $ Input v Slider (Just label) Nothing name [("min", pack $ show l), ("max", pack $ show u), ("step", pack $ show s)])

instance ToHtml Double where
  toHtml = toHtml . (show :: Double -> Text)
  toHtmlRaw = toHtmlRaw . (show :: Double -> Text)

sliderR_ :: (Monad m) => Text -> Double -> Double -> Double -> Scoot m Double
sliderR_ label l u = mkScooter double show $ \name v ->
  (toHtml $ jsbify $ bootify $ Input v Slider (Just label) Nothing name [("min", pack $ show l), ("max", pack $ show u)])

stepsliderR_ :: (Monad m) => Text -> Double -> Double -> Double -> Double -> Scoot m Double
stepsliderR_ label l u s = mkScooter double show $ \name v ->
  (toHtml $ jsbify $ bootify $ Input v Slider (Just label) Nothing name [("min", pack $ show l), ("max", pack $ show u), ("step", pack $ show s)])

textbox_ :: (Monad m) => Text -> Text -> Scoot m Text
textbox_ label = mkScooter takeText id $ \name v ->
  (toHtml $ jsbify $ bootify $ Input v TextBox (Just label) Nothing name [])

instance (RealFrac a, Floating a) => ToHtml (Colour a) where
  toHtml = toHtml . sRGB24show
  toHtmlRaw = toHtmlRaw . sRGB24show

instance (RealFrac a, Floating a) => Show (Colour a) where
  show = sRGB24show

color_ :: (RealFrac a, Floating a, Monad m) => Text -> Colour a -> Scoot m (Colour a)
color_ label = mkScooter ((sRGB24read . unpack) <$> ((<>) <$> string "#" <*> Data.Attoparsec.Text.take 6)) (pack . sRGB24show) $ \name v ->
  (toHtml $ jsbify $ bootify $ Input v ColorPicker (Just label) Nothing name [])

dropdown_ :: (Monad m, Show a) => Parser a -> Text -> [(Text, a)] -> a -> Scoot m a
dropdown_ p label opts = mkScooter p show $ \name v ->
  (toHtml $ jsbify $ bootify $ Input v (Dropdown ((\(f,s) -> (f,show s)) <$> opts) (Just (show v))) (Just label) Nothing name [])

dropdown'_ :: (Monad m, Show a) =>
  Parser a -> (a -> Text) -> Text -> [Text] -> a -> Scoot m a
dropdown'_ p pr label opts = mkScooter p pr $ \name v ->
  (toHtml $ jsbify $ bootify $
   Input v (Dropdown' opts (Just (show v))) (Just label) Nothing name [])

instance ToHtml Bool where
  toHtml = toHtml . bool ("false" :: Text) "true"
  toHtmlRaw = toHtmlRaw . bool ("false" :: Text) "true"

checkbox_ :: (Monad m) => Text -> Bool -> Scoot m Bool
checkbox_ label = mkScooter ((=="true") <$> takeText) (bool "false" "true") $ \name v ->
  (toHtml $ jsbify $ bootify $ Input v (Checkbox v) (Just label) Nothing name [])

wrap :: (Html () -> Html ()) -> Scooter a -> Scooter a
wrap w (Scooter h f) = Scooter (w h) f

wrapScoot :: (Monad m) => (Html () -> Html ()) -> Scoot m a -> Scoot m a
wrapScoot w s = Scoot $ fmap (wrap w) (unscoot s)

cardifyScoot :: (Monad m) => [(Text,Text)] -> Html () -> Maybe Text -> Scoot m a -> Scoot m a
cardifyScoot atts h mt s = Scoot $ fmap (wrap card) (unscoot s) where
  card = cardify atts h mt

checkboxShowJs :: (Monad m) => Text -> Text -> Bool -> Scoot m Bool
checkboxShowJs label cl = mkScooter ((=="true") <$> takeText) (bool "false" "true") $
  \name v ->
      (toHtml $
      showJsInput cl name $
      jsbify $
      bootify $
      Input v (Checkbox v) (Just label) Nothing name [])

maybeInput :: (Monad m) => Text -> Text -> Bool -> Scoot m a -> Scoot m (Maybe a)
maybeInput label cl start sa = do
  c <- checkboxShowJs label cl start
  a' <- wrapScoot (with div_ [class_ cl, style_ ("display:" <> bool "none" "block" start)]) sa
  pure (bool Nothing (Just a') c)

maybeInput' :: (Monad m) => Text -> Text -> Bool -> Scoot m a -> Scoot m (Maybe a)
maybeInput' label cl start sa = Scoot $ do
  (Scooter checkHtml checkFa) <- unscoot $ checkboxShowJs label cl start
  (Scooter bodyHtml bodyFa) <- unscoot sa
  pure $ Scooter
    (cardify [] checkHtml Nothing
     (with div_
       [class_ cl, style_
                   ("display:" <> bool "none" "block" start)]
       bodyHtml))
    (\hm -> either Left (bool (Right Nothing) (Just <$> bodyFa hm)) (checkFa hm))




