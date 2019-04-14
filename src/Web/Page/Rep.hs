{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Web.Page.Rep
  ( Element(..)
  , RepF(..)
  , Rep
  , SharedRepF(..)
  , SharedRep
  , closeRep
  , repInput
  , namedRepInput
  , sliderI
  , slider
  , dropdown
  , color
  , fromHex
  , toHex
  , PixelRGB8(..)
  , textbox
  , checkbox
  , toggle
  , button
  , buttonB
  , maybeRep
  , listify
  , accordionListify
  , execValue
  , execValueConsume
  , evalShared
  , runShared
  , runSharedRecord
  , evalSharedRepOnEvent
  , runSharedRepOnEvent
  ) where

import Codec.Picture.Types (PixelRGB8(..))
import Control.Applicative (liftA2)
import Control.Category (id)
import Control.Lens
import Control.Monad.Morph
import Data.Aeson
-- import Data.Aeson.Types hiding (Parser)
import Data.Attoparsec.Text
import Data.Biapplicative
import Data.Bifunctor (Bifunctor(..))
import Data.HashMap.Strict hiding (foldr)
import Data.Text (pack, Text)
import Lucid
-- import Network.JavaScript hiding (delete)
import Numeric
import Protolude hiding ((<<*>>), Rep, empty)
import Web.Page.Bootstrap
import Web.Page.Html
import Web.Page.Html.Input
import Box
import Box.Cont ()
-- import qualified Control.Foldl as L
import qualified Streaming.Prelude as S
-- import qualified Streaming as S

-- | Abstracted message event elements
data Element = Element
  { element :: Text
  , value :: Text
  } deriving (Eq, Show, Generic)

instance ToJSON Element

instance FromJSON Element
  where
    parseJSON = withObject "Element" $ \v ->
      Element <$>
      v .: "element" <*>
      v .: "value"

toEither :: Data.Aeson.Result a -> Either Text a
toEither (Success a) = Right a
toEither (Error e) = Left $ pack e

data RepF r a = Rep
  { rep :: r
  , make :: HashMap Text Text -> (HashMap Text Text, Either Text a)
  } deriving (Functor)

type Rep a = RepF (Html ()) a

instance (Semigroup r) => Semigroup (RepF r a) where
  (Rep r0 a0) <> (Rep r1 a1) =
    Rep
    (r0 <> r1)
    (\hm -> let (hm', x') = a0 hm in let (hm'', x'') = a1 hm' in (hm'', x' <> x''))

instance (Monoid a, Monoid r) => Monoid (RepF r a) where
  mempty = Rep mempty (\hm -> (hm, Right mempty))
  mappend = (<>)

instance Bifunctor RepF where
  bimap f g (Rep r a) = Rep (f r) (second (fmap g) . a)

instance Biapplicative RepF where
  bipure r a = Rep r (\hm -> (hm, Right a))
  (Rep fr fa) <<*>> (Rep r a) = Rep (fr r)
    (\hm ->
        let (hm', a') = a hm in let (hm'', fa') = fa hm' in (hm'', fa' <*> a'))

instance (Monoid r) => Applicative (RepF r) where
  pure = bipure mempty
  Rep fh fm <*> Rep ah am =
    Rep (fh <> ah) (\hm ->
        let (hm', a') = am hm in let (hm'', fa') = fm hm' in (hm'', fa' <*> a'))

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

closeRep :: (Functor m, ToHtml r) => SharedRepF m r a -> SharedRep m a
closeRep = first toHtml

repInput :: (ToHtml a, Monad m) => Parser a -> (a -> Text) -> Input a -> a -> SharedRepF m (Input a) a
repInput p pr i a =
  SharedRep $ do
    name <- zoom _1 genName
    zoom _2 (modify (insert name (pr a)))
    pure $
      Rep
      (bridgeify $ bootify $ set #val a $ set #id' name i)
      (\s ->
        (s, join $
        maybe (Left "lookup failed") Right $
        either (Left . (\x -> name <> ": " <> x) . pack) Right . parseOnly p <$> lookup name s))

namedRepInput :: (ToHtml a, MonadState (Int, HashMap Text Text) m, Monad m) =>
  Parser a -> (a -> Text) -> Input a -> a -> SharedRepF m (Input a) (Text, a) 
namedRepInput p pr i a = SharedRep $ do
    name <- zoom _1 genName
    zoom _2 (modify (insert name (pr a)))
    pure $ Rep
      (bridgeify $ bootify $ set #val a $ set #id' name i)
      (\s ->
          (s, fmap (\x -> (name,x)) $
              join $
              maybe (Left "lookup failed") Right $
              either (Left . pack) Right . parseOnly p <$> lookup name s))

-- | does not put a value into the HashMap on instantiation, consumes the value when found in the HashMap, and substitutes a default on lookup failure
repMessage :: (ToHtml a, Monad m) => Parser a -> (a -> Text) -> Input a -> a -> a -> SharedRepF m (Input a) a
repMessage p _ i def a =
  SharedRep $ do
    name <- zoom _1 genName
    pure $
      Rep
      (bridgeify $ bootify $ set #val a $ set #id' name i)
      (\s ->
        (delete name s, join $
        maybe (Right $ Right def) Right $
        either (Left . pack) Right . parseOnly p <$> lookup name s))

sliderF :: (Monad m) => Text -> Double -> Double -> Double -> Double ->
  SharedRepF m (Input Double) Double
sliderF label l u s v = repInput double show
  (Input v Slider (Just label) Nothing mempty [("min", pack $ show l), ("max", pack $ show u), ("step", pack $ show s)]) v

slider :: (Monad m) => Text -> Double -> Double -> Double -> Double ->
  SharedRep m Double
slider label l u s v = closeRep (sliderF label l u s v)

sliderIF :: (Monad m, ToHtml a, Integral a, Show a) => Text -> a -> a -> a -> a ->
  SharedRepF m (Input a) a
sliderIF label l u s v = repInput decimal show 
  (Input v Slider (Just label) Nothing mempty [("min", pack $ show l), ("max", pack $ show u), ("step", pack $ show s)]) v

sliderI :: (Monad m, ToHtml a, Integral a, Show a) => Text -> a -> a -> a -> a ->
  SharedRep m a
sliderI label l u s v = closeRep (sliderIF label l u s v)

textboxF :: (Monad m) => Text -> Text -> SharedRepF m (Input Text) Text
textboxF label v = repInput takeText id
  (Input v TextBox (Just label) Nothing mempty []) v

textbox :: (Monad m) => Text -> Text -> SharedRep m Text
textbox label v = closeRep (textboxF label v)

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

colorF :: (Monad m) => Text -> PixelRGB8 -> SharedRepF m (Input PixelRGB8) PixelRGB8
colorF label v = repInput fromHex toHex
  (Input v ColorPicker (Just label) Nothing mempty []) v

color :: (Monad m) => Text -> PixelRGB8 -> SharedRep m PixelRGB8
color label v = closeRep (colorF label v)

dropdownF :: (Monad m, ToHtml a) =>
  Parser a -> (a -> Text) -> Text -> [Text] -> a -> SharedRepF m (Input a) a
dropdownF p pr label opts v = repInput p pr 
  (Input v (Dropdown opts (Just (pr v))) (Just label) Nothing mempty []) v

dropdown :: (Monad m, ToHtml a) =>
  Parser a -> (a -> Text) -> Text -> [Text] -> a -> SharedRep m a
dropdown p pr label opts v = closeRep (dropdownF p pr label opts v)

checkboxF :: (Monad m) => Text -> Bool -> SharedRepF m (Input Bool) Bool
checkboxF label v = repInput ((=="true") <$> takeText) (bool "false" "true")
  (Input v (Checkbox v) (Just label) Nothing mempty []) v

checkbox :: (Monad m) => Text -> Bool -> SharedRep m Bool
checkbox label v = closeRep (checkboxF label v)

toggleF :: (Monad m) => Text -> Bool -> SharedRepF m (Input Bool) Bool
toggleF label v = repInput ((=="true") <$> takeText) (bool "false" "true")
  (Input v (Toggle v label) Nothing Nothing mempty []) v

toggle :: (Monad m) => Text -> Bool -> SharedRep m Bool
toggle label v = closeRep (toggleF label v)

instance ToHtml () where
  toHtml = const "()"
  toHtmlRaw = const "()"

buttonBF :: (Monad m) => Text -> SharedRepF m (Input Bool) Bool
buttonBF label = repMessage (pure True) (bool "false" "true")
  (Input False (Button label) Nothing Nothing mempty []) False False

buttonB :: (Monad m) => Text -> SharedRep m Bool
buttonB label = closeRep $ buttonBF label

buttonF :: (Monad m) => Text -> Text -> SharedRepF m (Input Text) Text
buttonF def label = repMessage takeText show
  (Input label (Button label) Nothing Nothing mempty []) def label

button :: (Monad m) => Text -> Text -> SharedRep m Text
button def label = closeRep $ buttonF def label

checkboxShowJs :: (Monad m) => Text -> Text -> Bool -> SharedRepF m (Input Bool) Bool
checkboxShowJs label cl v =
  SharedRep $ do
    name <- zoom _1 genName
    zoom _2 (modify (insert name (bool "false" "true" v)))
    pure $
      Rep
      ( showJsInput cl name $
        bridgeify $
        bootify $
        Input v (Checkbox v) (Just label) Nothing name [])
      (\s ->
        (s, join $
        maybe (Left "lookup failed") Right $
        either (Left . pack) Right . parseOnly ((=="true") <$> takeText) <$>
        lookup name s))

maybeRep :: (Monad m) => Text -> Bool -> SharedRep m a ->
  SharedRep m (Maybe a)
maybeRep label st sa = SharedRep $ do
  className <- zoom _1 genName
  sr <- unrep $ bimap (hmap className) mmap (closeRep $ checkboxShowJs label className st) <<*>> sa
  pure sr
  where
    hmap cl a b =
      cardify [] a Nothing
       (Lucid.with div_
        [class_ cl, style_
                   ("display:" <> bool "none" "block" st)]
        b)
    mmap a b = bool Nothing (Just b) a

listify :: (Monad m) => (Text -> a -> SharedRep m a) -> [Text] -> [a] -> SharedRep m [a]
listify sr labels as = foldr (\a x -> (:) <$> a <*> x) (pure []) (zipWith sr labels as)

accordionListify :: (Monad m) => Maybe Text -> Text -> Maybe Text -> (Text -> a -> SharedRep m a) -> [Text] -> [a] -> SharedRep m [a]
accordionListify title prefix open srf labels as = SharedRep $ do
  (Rep h fa) <-
    unrep $
    first (accordion prefix open . zipWith (,) labels ) $
    foldr (\a x -> bimap (:) (:) a <<*>> x)
    (pure []) (zipWith srf labels as)
  h' <- zoom _1 h
  pure (Rep (maybe mempty (h5_ . toHtml) title <> h') fa)

execValue :: (FromJSON a, MonadState s m) => (a -> s -> s) -> S.Stream (S.Of Value) m () -> S.Stream (S.Of (Either Text s)) m ()
execValue step s =
  s &
  S.map (toEither . fromJSON) &
  S.partitionEithers &
  hoist (S.chain (modify . step)) &
  hoist (S.mapM (\_ -> get)) &
  S.unseparate &
  S.maps S.sumToEither

execValueConsume :: s -> (Element -> s -> s) -> Cont IO (Committer IO (Either Text s)) -> Cont_ IO Value -> IO s
execValueConsume init step comm vio = do
  (c,e) <- atomically $ ends Unbounded
  with_ vio (atomically . c)
  final <- etcM init (Transducer (execValue step))
    (Box <$> comm <*> (liftE <$> pure (Emitter (Just <$> e))))
  pure final

shStep :: MonadState s m => (s -> (s, b)) -> (a -> s -> s) -> a -> m (s, b)
shStep sr step v = do
  hm <- get
  let (hm',b) = sr $ step v hm
  put hm'
  pure (hm', b)

evalShared :: (FromJSON a, MonadState s m) => (s -> (s, Either Text b)) -> (a -> s -> s) -> S.Stream (S.Of Value) m () -> S.Stream (S.Of (Either Text b)) m ()
evalShared sr step s =
  s &
  S.map (toEither . fromJSON) &
  S.partitionEithers &
  hoist (S.mapM (fmap snd . shStep sr step)) &
  S.unseparate &
  S.maps S.sumToEither &
  S.map join

runShared :: (FromJSON a, MonadState s m) => (s -> (s, Either Text b)) -> (a -> s -> s) -> S.Stream (S.Of Value) m () -> S.Stream (S.Of (Either Text (s, Either Text b))) m ()
runShared sr step s =
  s &
  S.map (toEither . fromJSON) &
  S.partitionEithers &
  hoist (S.mapM (shStep sr step)) &
  S.unseparate &
  S.maps S.sumToEither
 
runSharedRecord
  :: FromJSON a
  => MonadState (s, [Either Text (s, Either Text b)]) m
  => (s -> (s, Either Text b))
  -> (a -> s -> s)
  -> S.Stream (S.Of Value) m ()
  -> S.Stream (S.Of (Either Text (s, Either Text b))) m ()
runSharedRecord sr step s =
  s &
  S.map (toEither . fromJSON) &
  S.partitionEithers &
  hoist (S.mapM (shStep' sr step)) &
  S.unseparate &
  S.maps S.sumToEither &
  S.chain (\x -> modify (\(h,xs) -> (h, x:xs)))
  where
    shStep' sr' step' v = do
      (hm, xs) <- get
      let (hm',b) = sr' $ step' v hm
      put (hm', xs)
      pure (hm', b)

evalSharedEventConsume :: (s -> (s,Either Text b)) -> s -> (Element -> s -> s) -> Cont IO (Committer IO (Either Text b)) -> Cont_ IO Value -> IO s
evalSharedEventConsume sh init step comm vio = do
  (c,e) <- atomically $ ends Unbounded
  with_ vio (atomically . c)
  final <- etcM init (Transducer (evalShared sh step))
    (Box <$> comm <*> (liftE <$> pure (Emitter (Just <$> e))))
  pure final

runSharedEventConsume :: (s -> (s, Either Text b)) -> s -> (Element -> s -> s) -> Cont IO (Committer IO (Either Text (s, Either Text b))) -> Cont_ IO Value -> IO s
runSharedEventConsume sh init step comm vio = do
  (c,e) <- atomically $ ends Unbounded
  with_ vio (atomically . c)
  final <- etcM init (Transducer (runShared sh step))
    (Box <$> comm <*> (liftE <$> pure (Emitter (Just <$> e))))
  pure final

evalOnEvent
  :: (HashMap Text Text -> (HashMap Text Text, Either Text a))
  -> HashMap Text Text
  -> (Either Text a -> IO ())
  -> Cont_ IO Value
  -> IO (HashMap Text Text)
evalOnEvent sh init eaction =
  evalSharedEventConsume sh init (\(Element k v) s -> insert k v s)
  (pure (Committer (\v -> eaction v >> pure True)))

runOnEvent
  :: (HashMap Text Text -> (HashMap Text Text, Either Text a))
  -> HashMap Text Text
  -> (Either Text (HashMap Text Text, Either Text a) -> IO ())
  -> Cont_ IO Value
  -> IO (HashMap Text Text)
runOnEvent sh init eaction =
  runSharedEventConsume sh init (\(Element k v) s -> insert k v s)
  (pure (Committer (\v -> eaction v >> pure True)))

evalOnEventM
  :: (MonadState (HashMap Text Text) m)
  => (MonadIO m)
  => (HashMap Text Text -> (HashMap Text Text, Either Text a))
  -> (Either Text a -> IO ())
  -> Cont_ IO Value
  -> m ()
evalOnEventM sh eaction vio = do
  hm <- get
  res <- liftIO $ evalOnEvent sh hm eaction vio
  put res

runOnEventM
  :: (MonadState (HashMap Text Text) m)
  => (MonadIO m)
  => (HashMap Text Text -> (HashMap Text Text, Either Text a))
  -> (Either Text (HashMap Text Text, Either Text a) -> IO ())
  -> Cont_ IO Value
  -> m ()
runOnEventM sh eaction ev = do
  hm <- get
  res <- liftIO $ runOnEvent sh hm eaction ev
  put res

evalSharedRepOnEvent
  :: MonadIO m
  => SharedRep m a
  -> (Html () -> (HashMap Text Text -> (HashMap Text Text, Either Text a))
     -> StateT (Int, HashMap Text Text) m ())
  -> StateT (Int, HashMap Text Text) m ()
  -> (Either Text a -> IO ())
  -> Cont_ IO Value
  -> m ()
evalSharedRepOnEvent sr hio finalio eaction ev = flip evalStateT (0, empty) $ do
  Rep h fa <- unrep sr
  hio h fa
  zoom _2 $ evalOnEventM fa eaction ev
  finalio

runSharedRepOnEvent
  :: MonadIO m
  => SharedRep m a
  -> (Html () -> (HashMap Text Text -> (HashMap Text Text, Either Text a))
     -> StateT (Int, HashMap Text Text) m ())
  -> StateT (Int, HashMap Text Text) m ()
  -> (Either Text (HashMap Text Text, Either Text a) -> IO ())
  -> Cont_ IO Value
  -> m ()
runSharedRepOnEvent sr hio finalio eaction ev = flip evalStateT (0, empty) $ do
  Rep h fa <- unrep sr
  hio h fa
  zoom _2 $ runOnEventM fa eaction ev
  finalio

