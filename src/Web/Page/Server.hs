{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLabels #-}
{-# OPTIONS_GHC -Wall #-}

module Web.Page.Server
  ( servePageWith
  ) where

import Web.Page.Types
import Web.Page.Render
import Lucid (renderText)
import Control.Monad
import Web.Scotty
import Network.Wai.Middleware.Static (addBase, noDots, staticPolicy, only)
import qualified Data.Text.Lazy as Lazy
import qualified Control.Monad.State as State
import Lens.Micro

servePageWith :: RoutePattern -> PageConfig -> Page -> ScottyM ()
servePageWith rp pc p =
  sequence_ $ servedir <> [pages]
    where
  pages = case pc ^. #concerns of
    Inline -> get rp $  Web.Scotty.html $
           renderText $ renderPageHtmlWith pc p
    Separated -> let (css,js,h) = renderPageWith pc p in
      do
        -- middleware $ staticPolicy (noDots >-> addBase (takeDirectory cssfp))
        middleware $ staticPolicy $ only [(cssfp,cssfp), (jsfp, jsfp)]
        get rp (do
                   State.lift $ writeFile' cssfp (Lazy.unpack css)
                   State.lift $ writeFile' jsfp (Lazy.unpack js)
                   html $ renderText h)
  cssfp = pc ^. #filenames . #css
  jsfp = pc ^. #filenames . #js
  writeFile' fp s = unless (s == mempty) (writeFile fp s)
  servedir = (\x -> middleware $ staticPolicy (noDots <> addBase x)) <$> pc ^. #localdirs

