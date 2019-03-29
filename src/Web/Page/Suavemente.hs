{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall #-}

module Web.Page.Suavemente where

import Protolude hiding (replace)
import Control.Applicative (liftA2)
import Control.Concurrent.STM.TVar (newTVar, readTVar, writeTVar, TVar)
import Control.Monad.STM (STM, atomically)
import Control.Monad.State (StateT (..), liftIO)
import Control.Monad.State.Class (MonadState (..), modify)
import Control.Monad.Trans.Class (lift)
import Data.Aeson (FromJSON (..), Value (), fromJSON)
import Data.Text (pack, Text)
import Lucid
import Web.Page.Html.Input
import qualified Streaming as S
import qualified Streaming.Prelude as S
import Web.Page.JSB
import Data.Attoparsec.Text
import Network.JavaScript
import Box
import qualified Control.Exception as E
import qualified Data.Text.Lazy as Lazy

-- | An applicative functor can introduce new markup, and hook it up to the
-- event stream.
data Input' a = Input'
  { -- | The markup of the input.
    _iHtml :: Html ()

    -- | A means of handling the event stream. The stream is of (name, value)
    -- pairs. An 'Input' is responsible for stripping its own events out of
    -- this stream.
    --
    -- The 'IO ()' action is to publish a change notification to the downstream
    -- computations.
  , _iFold :: S.Stream (S.Of Element) IO ()
           -> S.Stream (S.Of Element) IO ()

    -- | The current value of the 'Input'.
  , _iValue :: STM a
  } deriving Functor

instance Applicative Input' where
  pure = Input' mempty (const $ pure ()) . pure
  Input' fh ff fv <*> Input' ah af av =
    Input' (fh <> ah)
          (af . ff)
          (fv <*> av)

-- | An applicative functor capable of getting input from an HTML page.
newtype Suave a = Suave
  { suavely :: StateT Int STM (Input' a)
  } deriving Functor

instance Applicative Suave where
  pure = Suave . pure . pure
  Suave f <*> Suave a = Suave $ liftA2 (<*>) f a


------------------------------------------------------------------------------
-- | An existentialized 'Suave'.
data SomeSuave where
  SomeSuave :: (a -> Html ()) -> Suave a -> SomeSuave

-- | Generate a new name for an HTML element.
genName :: MonadState Int m => Text -> m Text
genName prefix = do
  s <- get
  modify (+1)
  pure $ prefix <> show s

-- | Constructor for building 'Suave' inputs that are backed by HTML elements.
mkInput
    :: Parser a
    -> (Text -> a -> Html ())
    -> a                        -- ^ The input's initial value.
    -> Suave a
mkInput p f a = Suave $ do
  name <- genName "Suave"
  tvar <- lift $ newTVar a
  pure $ Input' (f name a) (getEvents p tvar name) (readTVar tvar)

getEvents
    :: Parser a
    -> TVar a  -- ^ The underlying 'TVar' to publish changes to.
    -> Text  -- ^ The name of the HTML input.
    -> S.Stream (S.Of Element) IO ()
    -> S.Stream (S.Of Element) IO ()
getEvents p t n
  = S.mapMaybeM (
    \a@(Element i z) ->
       case i == n of
          True  -> do
            liftIO . either print (atomically . writeTVar t) . parseOnly p $ z
            pure Nothing
          False -> pure $ Just a
           )

------------------------------------------------------------------------------
-- | Create an input driven by an HTML slider.
slider_
    :: (Integral a, Show a, ToHtml a, Num a, FromJSON a)
    => Text  -- ^ label
    -> a       -- ^ min
    -> a       -- ^ max
    -> a       -- ^ initial value
    -> Suave a
slider_ label l u = mkInput decimal $ \name v ->
  (toHtml $ jsbify $ bootify $ Input v Range (Just label) Nothing name [("min", pack $ show l), ("max", pack $ show u)])

textbox_
    :: Text  -- ^ label
    -> Text  -- ^ initial value
    -> Suave Text
textbox_ label = mkInput takeText $ \name v ->
  (toHtml $ jsbify $ bootify $ Input v TextBox (Just label) Nothing name [])

annotate :: (ToHtml a) => a -> Text -> Html ()
annotate a c = p_ (toHtml c) <> hr_ mempty <> toHtml a

testSuave :: (Show a) => Suave a -> Event Value -> Engine -> IO ()
testSuave s ev e = do
    em <- eventEmitter ev e
    putStrLn ("post-em" :: Text)
    Input' _ f _ <- atomically $ evalStateT (suavely s) 0
    putStrLn ("post-f" :: Text)
    S.effects $ f $ valueToElement (toStreamM em)
    Input' _ _ a <- atomically $ evalStateT (suavely s) 0
    putStrLn ("post-a" :: Text)
    a0 <- atomically a
    replace e "results" (show a0)

eventEmitter :: Event Value -> Engine -> IO (Emitter IO Value)
eventEmitter ev _ = do
  (c,e) <- atomically $ ends Unbounded
  void $ addListener ev (atomically . c)
  pure (liftE $ Emitter (Just <$> e))

valueToElement :: S.Stream (S.Of Value) IO () -> S.Stream (S.Of Element) IO ()
valueToElement s = s & S.map (toEither . fromJSON) & S.partitionEithers & S.effects

suaveMid :: Suave a -> (Event Value -> Engine -> IO ()) -> Application -> Application
suaveMid s eeio = start $ \ ev e -> do
  liftIO $ do
    Input' h _ _ <- atomically $ evalStateT (suavely s) 0
    append e "inputs" (Lazy.toStrict $ renderText h)
  eeio ev e `E.finally` putStrLn ("jsbBox finalled" :: Text)

testSuave' :: (Show a) => Suave a -> Event Value -> Engine -> IO ()
testSuave' s ev e = do
    Input' _ f _ <- atomically $ evalStateT (suavely s) 0
    putStrLn ("post-f" :: Text)
    eventBox''
      s
      (liftC <$> toCommit
       (\stream ->
          stream &
          S.concat &
          f &
          S.print))
      ev e
    putStrLn ("post-a" :: Text)
    -- a0 <- atomically a
    -- replace e "results" (show a0)

eventBox'' :: (Show a) => Suave a -> Cont IO (Committer IO (Either Text Element)) -> Event Value -> Engine -> IO ()
eventBox'' s comm ev e = do
  (c,em) <- atomically $ ends Unbounded
  void $ addListener ev (\v -> do
                            atomically $ c v
                            Input' _ _ a <- atomically $ evalStateT (suavely s) 0
                            a0 <- atomically a
                            replace e "results" (show a0)
                            )
  n <- etcM 0 model
    (Box <$>
     comm <*>
     (liftE <$> pure (Emitter (Just <$> em))))
  print n

