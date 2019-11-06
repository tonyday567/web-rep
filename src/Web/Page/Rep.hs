{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wall #-}

module Web.Page.Rep
  ( Element(..)
  , RepF(..)
  , Rep
  , oneRep
  , SharedRepF(..)
  , SharedRep
  , runOnce
  , zeroState
  , listify
  , accordionListify
  , accordionListifyMaybe
  , listifyDefault
  , listifyMaybe
  , listifyMaybe'
  , defaultListifyLabels
  , valueModel
  , valueConsume
  , sharedModel
  , sharedConsume
  , runList
  , runOnEvent
  ) where

import Box
import Box.Cont ()
import Control.Lens
import Control.Monad.Morph
import Data.Aeson
import Data.Biapplicative
import Data.Bifunctor (Bifunctor(..))
import Data.HashMap.Strict hiding (foldr)
import Data.Text (pack, Text)
import Lucid
import Protolude hiding ((<<*>>), Rep, empty)
import Web.Page.Bootstrap
import qualified Control.Foldl as L
import qualified Streaming.Prelude as S

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

fromJson' :: (FromJSON a) => Value -> Either Text a
fromJson' v = case fromJSON v of
  (Success a) -> Right a
  (Error e) -> Left $ "Json conversion error: " <> pack e <> " of " <> show v

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
  mempty = Rep mempty (,Right mempty)
  mappend = (<>)

instance Bifunctor RepF where
  bimap f g (Rep r a) = Rep (f r) (second (fmap g) . a)

instance Biapplicative RepF where
  bipure r a = Rep r (, Right a)
  (Rep fr fa) <<*>> (Rep r a) = Rep (fr r)
    (\hm ->
        let (hm', a') = a hm in let (hm'', fa') = fa hm' in (hm'', fa' <*> a'))

instance (Monoid r) => Applicative (RepF r) where
  pure = bipure mempty
  Rep fh fm <*> Rep ah am =
    Rep (fh <> ah) (\hm ->
        let (hm', a') = am hm in let (hm'', fa') = fm hm' in (hm'', fa' <*> a'))

oneRep :: (Monad m, MonadIO m) => Rep a -> (Rep a -> HashMap Text Text -> m ()) -> StateT (HashMap Text Text) m (HashMap Text Text, Either Text a)
oneRep r@(Rep _ fa) action = do
  m <- get
  let (m',a) = fa m
  put m'
  lift $ action r m'
  pure (m',a)

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

runOnce
  :: (Monad m)
  => SharedRep m a
  -> (Html () -> HashMap Text Text -> m ())
  -> m (HashMap Text Text, Either Text a)
runOnce sr action = do
  (Rep h fa, (_, m)) <- flip runStateT (0, empty) $ unrep sr
  action h m
  pure (fa m)

zeroState
  :: (Monad m)
  => SharedRep m a
  -> m (Html (), (HashMap Text Text, Either Text a))
zeroState sr = do
  (Rep h fa, (_, m)) <- flip runStateT (0, empty) $ unrep sr
  pure (h, fa m)

listify :: (Monad m) => (Text -> a -> SharedRep m a) -> [Text] -> [a] -> SharedRep m [a]
listify sr labels as = foldr (\a x -> (:) <$> a <*> x) (pure []) (zipWith sr labels as)

accordionListify :: (Monad m) => Maybe Text -> Text -> Maybe Text -> (Text -> a -> SharedRep m a) -> [Text] -> [a] -> SharedRep m [a]
accordionListify title prefix open srf labels as = SharedRep $ do
  (Rep h fa) <-
    unrep $
    first (accordion prefix open . zip labels ) $
    foldr (\a x -> bimap (:) (:) a <<*>> x)
    (pure []) (zipWith srf labels as)
  h' <- zoom _1 h
  pure (Rep (maybe mempty (h5_ . toHtml) title <> h') fa)

accordionListifyMaybe :: (Monad m) => Maybe Text -> Text -> (a -> SharedRep m a) -> (Bool -> SharedRep m Bool) -> [Text] -> [(Bool, a)] -> SharedRep m [(Bool,a)]
accordionListifyMaybe title prefix bodyf checkf labels xs = SharedRep $ do
  (Rep h fa) <-
    unrep $
    first (accordionChecked prefix) $
    first (zipWith (\l (ch,a) -> (l,a,ch)) labels) $
    foldr (\a x -> bimap (:) (:) a <<*>> x)
    (pure [])
    ((\(ch, a) ->
         (bimap (,) (,)
          (checkf ch) <<*>> bodyf a)) <$> xs)
  h' <- zoom _1 h
  pure (Rep (maybe mempty (h5_ . toHtml) title <> h') fa)

listifyDefault :: (Monad m) => Maybe Text -> Text -> (Text -> a -> SharedRep m a) -> [a] -> SharedRep m [a]
listifyDefault t p srf as = accordionListify t p Nothing srf (defaultListifyLabels (length as)) as

listifyMaybe :: (Monad m) => Maybe Text -> Text -> (Text -> Maybe a -> SharedRep m (Maybe a)) -> Int -> [a] -> SharedRep m [Maybe a]
listifyMaybe t p srf n as = accordionListify t p Nothing srf (defaultListifyLabels n) (take n ((Just <$> as) <> repeat Nothing)) 

listifyMaybe' :: (Monad m) => Maybe Text -> Text -> (Bool -> SharedRep m Bool) -> (a -> SharedRep m a) -> Int -> a -> [a] -> SharedRep m [a]
listifyMaybe' t p brf srf n defa as = second (mconcat . fmap (\(b,a) -> bool [] [a] b)) $ accordionListifyMaybe t p srf brf (defaultListifyLabels n) (take n (((True,) <$> as) <> repeat (False, defa)))

defaultListifyLabels :: Int -> [Text]
defaultListifyLabels n = (\x -> "[" <> show x <> "]") <$> [0..n] :: [Text]

valueModel :: (FromJSON a, MonadState s m) => (a -> s -> s) -> S.Stream (S.Of Value) m () -> S.Stream (S.Of (Either Text s)) m ()
valueModel step s =
  s &
  S.map fromJson' &
  S.partitionEithers &
  hoist (S.chain (modify . step)) &
  hoist (S.mapM (const get)) &
  S.unseparate &
  S.maps S.sumToEither

-- | consume an Element using a Committer and a Value continuation
valueConsume :: s -> (Element -> s -> s) -> Cont IO (Committer IO (Either Text s)) -> Cont_ IO Value -> IO s
valueConsume init step comm vio = do
  (c,e) <- atomically $ ends Unbounded
  with_ vio (atomically . c)
  etcM init (Transducer (valueModel step))
    (Box <$> comm <*> (liftE <$> pure (Emitter (Just <$> e))))

stepM :: MonadState s m => (s -> (s, b)) -> (a -> s -> s) -> a -> m (s, b)
stepM sr step v = do
  hm <- get
  let (hm',b) = sr $ step v hm
  put hm'
  pure (hm', b)

sharedModel :: (FromJSON a, MonadState s m) => (s -> (s, Either Text b)) -> (a -> s -> s) -> S.Stream (S.Of Value) m () -> S.Stream (S.Of (Either Text (s, Either Text b))) m ()
sharedModel sr step s =
  s &
  S.map fromJson' &
  S.partitionEithers &
  hoist (S.mapM (stepM sr step)) &
  S.unseparate &
  S.maps S.sumToEither

runList
  :: (Monad m)
  => SharedRep m a
  -> [Value]
  -> m [Either Text (HashMap Text Text, Either Text a)]
runList sr vs = S.fst' <$> do
  (faStep, (_,hm)) <- flip runStateT (0, empty) $ do
    (Rep _ fa) <- unrep sr
    pure fa
  flip evalStateT hm $
    L.purely S.fold L.list
    (sharedModel faStep (\(Element k v) s -> insert k v s) (S.each vs))

sharedConsume :: (s -> (s, Either Text b)) -> s -> (Element -> s -> s) -> Cont IO (Committer IO (Either Text (s, Either Text b))) -> Cont_ IO Value -> IO s
sharedConsume sh init step comm vio = do
  (c,e) <- atomically $ ends Unbounded
  with_ vio (atomically . c)
  etcM init (Transducer (sharedModel sh step))
    (Box <$> comm <*> (liftE <$> pure (Emitter (Just <$> e))))

runOnEvent
  :: SharedRep IO a
  -> (Rep a -> StateT (Int, HashMap Text Text) IO ())
  -> (Either Text (HashMap Text Text, Either Text a) -> IO ())
  -> Cont_ IO Value
  -> IO (HashMap Text Text)
runOnEvent sr hio eaction cv = flip evalStateT (0, empty) $ do
  (Rep h fa) <- unrep sr
  hio (Rep h fa)
  m <- zoom _2 get
  liftIO $ sharedConsume fa m (\(Element k v) s -> insert k v s)
    (pure (Committer (\v -> eaction v >> pure True))) cv
