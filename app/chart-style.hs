{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

import Control.Lens
import Data.Aeson (Value)
import Data.Attoparsec.Text
import Data.HashMap.Strict
import Lucid hiding (b_)
import Network.JavaScript
import Network.Wai.Middleware.Static (addBase, noDots, staticPolicy, (>->))
import Protolude hiding (replace, empty, Rep)
import Web.Page
import Web.Page.Bootstrap
import Web.Page.Examples
import Web.Page.Html.Input
import Web.Page.Bridge
import Web.Page.Bridge.Rep
import Web.Page.Chart
import Web.Scotty
import qualified Box
import qualified Control.Exception as E
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Lazy
import Chart.Core
import Chart.Hud
import Chart.Spot
import Chart.Svg
import NumHask.Data.Range

testPage :: Html () -> Page
testPage h =
  showJs <>
  bootstrapPage <>
  bridgePage &
  #htmlHeader .~ title_ "testPage" &
  #htmlBody .~ b_ "container"
  (mconcat
    [ b_ "row" (h1_ "testPage")
    , b_ "row" (with div_ [id_ "test"] h)
    , b_ "row" (with div_ [id_ "log"] (h2_ "server log"))
    ])

ioTestPage :: Html () -> Html () -> Page
ioTestPage i r =
  showJs <>
  bootstrapPage <>
  bridgePage &
  #htmlHeader .~ title_ "ioTestPage" &
  #htmlBody .~ b_ "container"
  (mconcat
    [ b_ "row" (h1_ "input-output test page")
    , b_ "row" (b_ "col-sm" (h2_ "inputs" <> with form_ [id_ "inputs"] i) <>
           b_ "col-sm" (with div_ [id_ "output"]
                  (h2_ "output" <>
                   with div_ [id_ "results"] r)))
    , b_ "row" (with div_ [id_ "log"] (h2_ "server log"))
    ])

chartTest :: GlyphStyle -> HudConfig Double -> ChartSvg Double
chartTest gs cfg =
  hud cfg
  (aspect 1.5)
  [ Chart (GlyphA gs) mempty
    (SpotPoint <$> dataXY sin (Range 0 (2*pi)) 30)
  ]

results :: (a -> Text) -> Engine -> a -> IO ()
results r e x = replace e "results" (r x)

logResults :: (a -> Text) -> Engine -> Either Text a -> IO ()
logResults _ e (Left err) = append e "log" err
logResults r e (Right x) = results r e x

midChartTest :: (Show a) => SharedRep IO a -> (a -> Text) -> Application -> Application
midChartTest sc rend = start $ \ ev e -> do
  (Rep h fa, (_, hm)) <- flip runStateT (0,empty) (unrep sc)
  append e "inputs" (Lazy.toStrict $ renderText h)
  either (const $ append e "log" "fa hm stuffed up") (results rend e) (fa hm)
  final <- consumeSharedBridge (logResults rend) hm (either Left fa) ev e
    `E.finally` putStrLn ("finalled" :: Text)
  putStrLn $ ("final value was: " :: Text) <> show final

renderChart :: GlyphStyle -> HudConfig Double -> Text
renderChart gs cfg =
  xmlToText . renderXml (Point (600.0 :: Double) 400) $ chartTest gs cfg

renderChartGs :: GlyphStyle -> Text
renderChartGs gs = renderChart gs (defaultHudConfig & #title1 .~ Just (defaultTitle "glyph test"))

main :: IO ()
main =
  scotty 3000 $ do
    middleware $ staticPolicy (noDots >-> addBase "other")
    middleware 
      ( midChartTest (repChart defaultHudConfig)
        (\(gs,cfg) -> renderChart gs cfg))
    servePageWith "/chart/style" defaultPageConfig chartStylePage
