{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
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
import qualified Data.Text.Lazy as Lazy
import qualified Control.Monad.State as State
import Lens.Micro
import Network.Wai
import qualified Data.ByteString.Char8 as BS
import Web.Scotty.Internal.Types (RoutePattern(..))

isLitEq :: RoutePattern -> [Char] -> Bool
isLitEq (Literal l) t = Lazy.unpack l == t
isLitEq _ _ = False

servePageWith :: RoutePattern -> Middleware -> PageConfig -> Page -> ScottyM ()
servePageWith rp mid pc p =
  sequence_ $ servedir <> [getpage, mids]
    where
  mids =
    middleware $ ifRequest (\req -> rp `isLitEq` BS.unpack (rawPathInfo req)) mid
  getpage = case pc ^. #concerns of
    Inline ->
      get rp (html $ renderText $ renderPageHtmlWith pc p)
    Separated -> let (css,js,h) = renderPageWith pc p in
      do
        middleware $ staticPolicy $ only [(cssfp,cssfp), (jsfp, jsfp)]
        get rp (do
                   State.lift $ writeFile' cssfp (Lazy.toStrict css)
                   State.lift $ writeFile' jsfp (Lazy.toStrict js)
                   html $ renderText h)
  cssfp = pc ^. #filenames . #css
  jsfp = pc ^. #filenames . #js
  writeFile' fp s = unless (s == mempty) (writeFile fp s)
  servedir = (\x -> middleware $ staticPolicy (noDots <> addBase x)) <$> pc ^. #localdirs

