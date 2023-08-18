-- | A haskell library for representing web pages.
--
-- This library is a collection of web page abstractions, together with a reimagining of <http://hackage.haskell.org/package/suavemente suavemente>.
--
-- I wanted to expose the server delivery mechanism, switch the streaming nature of the gap between a web page and a haskell server, and concentrate on getting a clean interface between pure haskell and the world that is a web page.
--
-- See app/examples.hs and 'Web.Examples' for usage.
module Web.Rep
  ( -- * Shared Representation
    RepF (..),
    Rep,
    oneRep,
    SharedRepF (..),
    SharedRep,
    runOnce,
    zeroState,
    register,
    genName,
    genNamePre,

    -- * Web Rep Components
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
    Css (..),
    renderCss,

    -- * JS
    Js (..),
    onLoad,

    -- * re-export modules
    module Web.Rep.SharedReps,
    module Web.Rep.Render,
    module Web.Rep.Server,
    module Web.Rep.Socket,
    module Web.Rep.Html,
    module Web.Rep.Html.Input,
    module Web.Rep.Bootstrap,

    -- * re-exports
    HashMap.HashMap,
  )
where

import Data.HashMap.Strict qualified as HashMap
import Web.Rep.Bootstrap
import Web.Rep.Html
import Web.Rep.Html.Input
import Web.Rep.Page
import Web.Rep.Render
import Web.Rep.Server
import Web.Rep.Shared
import Web.Rep.SharedReps
import Web.Rep.Socket
