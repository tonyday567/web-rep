{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wall #-}

-- | Serve pages via 'ScottyM'
module Web.Page.Server
  ( servePageWith,
    defaultServePage,
    serveRep,
    initRep,
    updateRep,
  )
where

import Control.Lens hiding (only)
import Data.HashMap.Strict (HashMap)
import Lucid
import Network.Wai.Middleware.Static (addBase, noDots, only, staticPolicy)
import NumHask.Prelude hiding (get, replace)
import Web.Page.Bootstrap
import Web.Page.Bridge
import Web.Page.Html
import Web.Page.Mathjax
import Web.Page.Render
import Web.Page.Types
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
                    lift $ writeFile' cssfp css
                    lift $ writeFile' jsfp js
                    html $ renderText h
                )
    cssfp = pc ^. #filenames . #cssConcern
    jsfp = pc ^. #filenames . #jsConcern
    writeFile' fp s = unless (s == mempty) (writeFile fp s)
    servedir = (\x -> middleware $ staticPolicy (noDots <> addBase x)) <$> pc ^. #localdirs

defaultServePage :: Bool -> Page
defaultServePage doDebug =
  mathjaxSvgPage "hasmathjax"
    <> bootstrapPage
    <> bridgePage
    & #htmlHeader .~ title_ "stats.hs testing"
    & #htmlBody
      .~ div_
        [class__ "container"]
        ( div_
            [class__ "row no-gutters"]
            ( div_ [class__ "col-4", id_ "input"] mempty
                <> div_ [class__ "col-8", id_ "output"] mempty
            )
            <> bool mempty (div_ [class__ "row", id_ "debug"] mempty) doDebug
        )

serveRep :: SharedRep IO a -> Bool -> (a -> IO [Text]) -> IO ()
serveRep mRep debug makeRep =
  scotty 3000 $ do
    middleware $ midShared mRep (initRep makeRep) (updateRep makeRep)
    servePageWith "/" (defaultPageConfig "prod") (defaultServePage debug)

initRep :: (a -> IO [Text]) -> Engine -> Rep a -> StateT (HashMap Text Text) IO ()
initRep mr e r =
  void $
    oneRep
      r
      ( \(Rep h fa) m -> do
          append e "input" (toText h)
          case snd (fa m) of
            Left err -> append e "debug" err
            Right cfg -> do
              c <- mr cfg
              replace e "output" (mconcat c)
      )

updateRep :: (a -> IO [Text]) -> Engine -> Either Text (b, Either Text a) -> IO ()
updateRep _ e (Left err) =
  append e "debug" err
updateRep _ e (Right (_, Left err)) =
  append e "debug" err
updateRep mr e (Right (_, Right c)) = do
  t <- mr c
  replace e "output" (mconcat t)
