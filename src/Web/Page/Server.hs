{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLabels #-}
{-# OPTIONS_GHC -Wall #-}

-- | Serve pages via 'ScottyM'
module Web.Page.Server
  ( servePageWith
  ) where

import Control.Lens hiding (only)
import Lucid (renderText)
import Network.Wai.Middleware.Static (addBase, noDots, staticPolicy, only)
import Prelude
import Web.Page.Render
import Web.Page.Types
import Web.Scotty
import qualified Control.Monad.State as State
import Control.Monad
import Data.Text (unpack)

-- | serve a Page via a ScottyM
servePageWith :: RoutePattern -> PageConfig -> Page -> ScottyM ()
servePageWith rp pc p =
  sequence_ $ servedir <> [getpage]
    where
  getpage = case pc ^. #concerns of
    Inline ->
      get rp (html $ renderText $ renderPageHtmlWith pc p)
    Separated -> let (css,js,h) = renderPageWith pc p in
      do
        middleware $ staticPolicy $ only [(cssfp,cssfp), (jsfp, jsfp)]
        get rp (do
                   State.lift $ writeFile' cssfp (unpack css)
                   State.lift $ writeFile' jsfp (unpack js)
                   html $ renderText h)
  cssfp = pc ^. #filenames . #cssConcern
  jsfp = pc ^. #filenames . #jsConcern
  writeFile' fp s = unless (s == mempty) (writeFile fp s)
  servedir = (\x -> middleware $ staticPolicy (noDots <> addBase x)) <$> pc ^. #localdirs

