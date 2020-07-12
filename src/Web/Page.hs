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
    module Web.Page.Socket,
    module Web.Page.Html,
    module Web.Page.Html.Input,
    module Web.Page.Bootstrap,
    module Web.Page.Mathjax,

    -- * re-exports
    HashMap.HashMap,
  )
where

import qualified Data.HashMap.Strict as HashMap
import Web.Page.Bootstrap
import Web.Page.Socket
import Web.Page.Html
import Web.Page.Html.Input
import Web.Page.Mathjax
import Web.Page.Render
import Web.Page.Server
import Web.Page.SharedReps
import Web.Page.Types
