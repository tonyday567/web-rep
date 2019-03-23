{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall #-}

module Web.Page.Suavemente where

import Protolude
import Control.Applicative (liftA2)
import Control.Concurrent.STM.TVar (newTVar, readTVar, writeTVar, TVar)
import Control.Monad.STM (STM, atomically)
import Control.Monad.State (StateT (..), liftIO)
import Control.Monad.State.Class (MonadState (..), modify)
import Control.Monad.Trans.Class (lift)
import Data.Aeson (FromJSON (..), Value (), Result(..))
import Data.Aeson.Types (Parser, parse)
import Data.Text (pack, Text)
import GHC.Generics (Generic)
import Lucid
import Web.Page.Html.Input
import qualified Streaming as S
import qualified Streaming.Prelude as S

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
  , _iFold :: IO ()
           -> S.Stream (S.Of ChangeEvent) IO ()
           -> S.Stream (S.Of ChangeEvent) IO ()

    -- | The current value of the 'Input'.
  , _iValue :: STM a
  } deriving Functor

instance Applicative Input' where
  pure = Input' mempty (const . const $ pure ()) . pure
  Input' fh ff fv <*> Input' ah af av =
    Input' (fh <> ah)
          (liftA2 (.) ff af)
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

-- | Change messages that come from the JS side.
data ChangeEvent = ChangeEvent
  { ceElement :: Text
  , cePayload :: Value
  } deriving (Eq, Show, Generic)

-- | Generate a new name for an HTML element.
genName :: MonadState Int m => m Text
genName = do
  s <- get
  modify (+1)
  pure $ show s

-- | Constructor for building 'Suave' inputs that are backed by HTML elements.
mkInput
    :: (Value -> Parser a)
    -> (Text -> a -> Html ())
    -> a                        -- ^ The input's initial value.
    -> Suave a
mkInput p f a = Suave $ do
  name <- genName
  tvar <- lift $ newTVar a
  pure $ Input' (f name a) (getEvents p tvar name) (readTVar tvar)

-- | EXPLODE IF PARSING FAILS
fromResult :: Result a -> a
fromResult (Success a) = a
fromResult (Error s) = panic (pack s)

getEvents
    :: (Value -> Parser a)
    -> TVar a  -- ^ The underlying 'TVar' to publish changes to.
    -> Text  -- ^ The name of the HTML input.
    -> IO ()   -- ^ Publish a change notification.
    -> S.Stream (S.Of ChangeEvent) IO ()
    -> S.Stream (S.Of ChangeEvent) IO ()
getEvents p t n update
  = S.mapMaybeM (
    \a@(ChangeEvent i z) ->
       case i == n of
          True  -> do
            liftIO . atomically . writeTVar t . fromResult $ parse p z
            update
            pure Nothing
          False -> pure $ Just a
           )

------------------------------------------------------------------------------
-- | Create an input driven by an HTML slider.
slider_
    :: (Show a, ToHtml a, Num a, FromJSON a)
    => Text  -- ^ label
    -> a       -- ^ min
    -> a       -- ^ max
    -> a       -- ^ initial value
    -> Suave a
slider_ label l u = mkInput parseJSON $ \name v ->
  (toHtml $ Input v Range (Just label) Nothing name [("min", pack $ show l), ("max", pack $ show u)])

textbox_
    :: Text  -- ^ label
    -> Text  -- ^ initial value
    -> Suave Text
textbox_ label = mkInput parseJSON $ \name v ->
  (toHtml $ Input v TextBox (Just label) Nothing name [])

annotate :: (ToHtml a) => a -> Text -> Html ()
annotate a c = p_ (toHtml c) <> hr_ mempty <> toHtml a

