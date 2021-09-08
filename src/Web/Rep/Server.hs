{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLabels #-}
{-# OPTIONS_GHC -Wall #-}

-- | Serve pages via 'ScottyM'
module Web.Rep.Server
  ( servePageWith,
  )
where

import Control.Lens hiding (only)
import Control.Monad
import Control.Monad.Trans.Class
import Data.Text (unpack)
import Lucid
import Network.Wai.Middleware.Static (addBase, noDots, only, staticPolicy)
import Web.Rep.Page
import Web.Rep.Render
import Web.Scotty

-- | serve a Page via a ScottyM
servePageWith :: RoutePattern -> PageConfig -> Page -> ScottyM ()
servePageWith rp pc p =
  sequence_ $ servedir <> [getpage]
  where
    getpage = case pc ^. #concerns of
      Inline ->
        get rp (html $ renderText $ renderPageHtmlWith pc p)
      Separated ->
        let (css, js, h) = renderPageWith pc p
         in do
              middleware $ staticPolicy $ only [(cssfp, cssfp), (jsfp, jsfp)]
              get
                rp
                ( do
                    lift $ writeFile' cssfp (unpack css)
                    lift $ writeFile' jsfp (unpack js)
                    html $ renderText h
                )
    cssfp = pc ^. #filenames . #cssConcern
    jsfp = pc ^. #filenames . #jsConcern
    writeFile' fp s = unless (s == mempty) (writeFile fp s)
    servedir = (\x -> middleware $ staticPolicy (noDots <> addBase x)) <$> pc ^. #localdirs
