{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wall #-}

module Web.Page.Server
  ( servePageWith
  ) where

import Protolude hiding (get)
import Web.Page.Types
import Web.Page.Render
import Lucid (renderText)
import Web.Scotty
import Network.Wai.Middleware.Static (addBase, noDots, staticPolicy, only)
import qualified Control.Monad.State as State
import Control.Lens hiding (only)

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
                   State.lift $ writeFile' cssfp css
                   State.lift $ writeFile' jsfp js
                   html $ renderText h)
  cssfp = pc ^. #filenames . #css
  jsfp = pc ^. #filenames . #js
  writeFile' fp s = unless (s == mempty) (writeFile fp s)
  servedir = (\x -> middleware $ staticPolicy (noDots <> addBase x)) <$> pc ^. #localdirs

