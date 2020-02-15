-- | A haskell library for representing web pages.
--
-- This library is a collection of web page abstractions, together with a reimagining of <http://hackage.haskell.org/package/suavemente suavemente>.
--
-- I wanted to expose the server delivery mechanism, switch the streaming nature of the gap between a web page and a haskell server, and concentrate on getting a clean interface between pure haskell and the world that is a web page.
--
-- See app/examples.hs and 'Web.Examples' for usage.
module Web.Page
  ( -- * Shared Representation
    RepF (..),
    Rep,
    oneRep,
    SharedRepF (..),
    SharedRep,
    Element (..),
    runOnce,
    zeroState,

    -- * Web Page Components
    Page (..),
    PageConfig (..),
    defaultPageConfig,
    Concerns (..),
    suffixes,
    concernNames,
    PageConcerns (..),
    PageStructure (..),
    PageRender (..),

    -- * Css
    Css,
    PageCss (..),
    renderCss,
    renderPageCss,

    -- * JS
    JS (..),
    PageJs (..),
    onLoad,
    renderPageJs,
    parseJs,
    renderJs,

    -- * re-export modules
    module Web.Page.SharedReps,
    module Web.Page.Render,
    module Web.Page.Server,
    module Web.Page.Bridge,
    module Web.Page.Html,
    module Web.Page.Html.Input,
    module Web.Page.Bootstrap,
    module Web.Page.Mathjax,
    module Data.Biapplicative,
    module Data.Bifunctor,

    -- * re-exports
    module X,
    Value (..),
    finally,
    PixelRGB8 (..),
    HashMap.HashMap,
    fromList,
    void,
    sequenceA_,
    Text,
    pack,
    unpack,
    toStrict,
    bool,
  )
where

import Codec.Picture.Types (PixelRGB8 (..))
import Control.Applicative as X
import Control.Exception (finally)
import Control.Monad
import Control.Monad.Trans.State as X
import Data.Aeson (Value (..))
import Data.Biapplicative
import Data.Bifunctor
import Data.Bool
import Data.Foldable (sequenceA_)
import qualified Data.HashMap.Strict as HashMap
import Data.Text (Text, pack, unpack)
import Data.Text.Lazy (toStrict)
import GHC.Exts (fromList)
import Text.InterpolatedString.Perl6 as X
import Web.Page.Bootstrap
import Web.Page.Bridge
import Web.Page.Html
import Web.Page.Html.Input
import Web.Page.Mathjax
import Web.Page.Render
import Web.Page.Server
import Web.Page.SharedReps
import Web.Page.Types
