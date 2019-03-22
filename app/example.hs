{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wall #-}

import Web.Page
import Web.Page.Html.Input
import Lucid
import qualified Clay
import Protolude hiding (replace)
import Network.JavaScript
import Web.Scotty
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Lazy
import Lens.Micro
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.Wai.Middleware.Static (addBase, noDots, staticPolicy, (>->))
import Data.Aeson
import Data.Aeson.Types
import Text.InterpolatedString.Perl6
import qualified Control.Exception as E

page1 :: Page
page1 =
  #htmlBody .~ Main.button $
  #cssBody .~ css $
  #jsGlobal .~ mempty $
  #jsOnLoad .~ click $
  #libsCss .~ (libCss <$> cssLibs) $
  #libsJs .~ (libJs <$> jsLibs) $
  mempty

page2 :: Page
page2 =
  #libsCss .~ (libCss <$> cssLibsLocal) $
  #libsJs .~ (libJs <$> jsLibsLocal) $
  page1

cfg2 :: PageConfig
cfg2 =
  #concerns .~ Separated $
  #pageRender .~ Pretty $
  #structure .~ Headless $
  #localdirs .~ ["test/static"] $
  #filenames .~ (("other/cfg2" <>) <$> suffixes) $
  defaultPageConfig

cssLibs :: [Text]
cssLibs =
  ["http://maxcdn.bootstrapcdn.com/font-awesome/4.3.0/css/font-awesome.min.css"]

cssLibsLocal :: [Text]
cssLibsLocal = ["css/font-awesome.min.css"]

jsLibs :: [Text]
jsLibs = ["http://code.jquery.com/jquery-1.6.3.min.js"]

jsLibsLocal :: [Text]
jsLibsLocal = ["jquery-2.1.3.min.js"]

css :: Css
css = do
  Clay.fontSize (Clay.px 10)
  Clay.fontFamily ["Arial", "Helvetica"] [Clay.sansSerif]
  "#btnGo" Clay.? do
    Clay.marginTop (Clay.px 20)
    Clay.marginBottom (Clay.px 20)
  "#btnGo.on" Clay.? Clay.color Clay.green

-- js
click :: PageJs
click = PageJsText [q|
$('#btnGo').click( function() {
  $('#btnGo').toggleClass('on');
  alert('bada bing!');
});
|]

button :: Html ()
button =
  with
    button_
    [id_ "btnGo", Lucid.type_ "button"]
    ("Go " <> with i_ [class_ "fa fa-play"] mempty)


bootstrapCss :: [Html ()]
bootstrapCss =
  [link_
  [ rel_ "stylesheet"
  , href_ "https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/css/bootstrap.min.css"
  , integrity_ "sha384-ggOyR0iXCbMQv3Xipma34MD+dH/1fQ784/j6cY/iJTQUOhcWr7x9JvoRxT2MZw1T"
  , crossorigin_ "anonymous"
  ]]

bootstrapJs :: [Html ()]
bootstrapJs =
  [ with (script_ mempty)
    [ src_ "https://code.jquery.com/jquery-3.3.1.slim.min.js"
    , integrity_ "sha384-q8i/X+965DzO0rT7abK41JStQIAqVgRVzpbzo5smXKp4YfRvH+8abtTE1Pi6jizo"
    , crossorigin_ "anonymous"
    ]
  , with (script_ mempty)
    [ src_ "https://cdnjs.cloudflare.com/ajax/libs/popper.js/1.14.7/umd/popper.min.js"
    , integrity_ "sha384-UO2eT0CpHqdSJQ6hJty5KVphtPhzWj9WO1clHTMGa3JDZwrnQq4sF86dIHNDz0W1"
    , crossorigin_ "anonymous"
    ]
  , with (script_ mempty)
    [ src_ "https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/js/bootstrap.min.js"
    , integrity_ "sha384-JjSmVgyd0p3pXB1rRibZUAYoIIy6OrQ6VrjIEaFf/nJGzIxFDsf4x0xIM+B07jRM"
    , crossorigin_ "anonymous"
    ]
  ]

bootstrapMeta :: [Html ()]
bootstrapMeta =
  [ meta_ [charset_ "utf-8"]
  , meta_ [ name_ "viewport"
          , content_ "width=device-width, initial-scale=1, shrink-to-fit=no"]
  ]

jsbPreventEnter :: PageJs
jsbPreventEnter = PageJs $ fromText [q|
window.addEventListener('keydown',function(e) {
  if(e.keyIdentifier=='U+000A' || e.keyIdentifier=='Enter' || e.keyCode==13) {
    if(e.target.nodeName=='INPUT' && e.target.type !== 'textarea') {
      e.preventDefault();
      return false;
    }
  }
}, true);
|]

jsbWebSocket :: PageJs
jsbWebSocket = PageJsText [q|
window.jsb = {ws: new WebSocket('ws://' + location.host + '/')};
jsb.ws.onmessage = (evt) => eval(evt.data);
|]

bootstrapPage :: Page
bootstrapPage =
  Page
  bootstrapCss
  bootstrapJs
  mempty
  mempty
  mempty
  (mconcat bootstrapMeta)
  mempty

jsbPage :: Page
jsbPage =
  mempty &
  #jsGlobal .~ jsbPreventEnter &
  #jsOnLoad .~ jsbWebSocket

inputTest :: Page
inputTest =
  bootstrapPage <>
  jsbPage &
  #htmlHeader .~ title_ "input development" &
  #htmlBody .~ mconcat
  [ h1_ "inputs"
  , with div_ [style_ "padding:1.5rem;position:relative;border-style:solid;border-color:#f8f9fa;border-width:0.2rem;"]
    (h2_ "inputs" <> with form_ [id_ "inputs"] mempty)
  , with div_ [id_ "output"] (h2_ "output" <> with div_ [id_ "results"] mempty)
  , with div_ [id_ "log"] (h2_ "server log")
  ]

rangeTest :: Input Int
rangeTest = jsbify $ bootify $
  Input
  3
  Range
  (Just "range example")
  Nothing
  "rangeid"
  [ ("style", "max-width:15rem;")
  , ("min", "0")
  , ("max", "5")
  , ("step", "1")
  ]

textTest :: Input Text
textTest = jsbify $ bootify $
  Input
  "abc"
  TextBox
  (Just "label")
  Nothing
  "textid"
  [ ("style", "max-width:15rem;")
  , ("placeholder", "test placeholder")
  ]

eventListener :: Int -> Event Value -> Engine -> IO ()
eventListener timeout ev e = do
  putStrLn ("runner (re)started" :: Text)
  append e "log" "(re)start ..."
  let x = renderText (toHtml rangeTest <> toHtml textTest)
  append e "inputs" (Lazy.toStrict x)
  _ <- addListener ev (result e)
  threadDelay (timeout * 1000 * 1000)
  replace e "log" "timed out"

{-
echo :: Value -> IO ()
echo v = void $ with (cStdout 100) $ \c -> atomically (commit c (show (fromJSON v :: Result Value) :: Text))
-}

result :: Engine -> Value -> IO ()
result e v =
  either
  (append e "log" . Text.pack)
  (replace e "results" . (show :: Element -> Text))
  (parseEither parseJSON v)

replace :: Engine -> Text -> Text -> IO ()
replace e d t = send e $ command $ Lazy.fromStrict $ "document.getElementById('" <> d <> "').innerHTML = '" <> t <> "'"

append :: Engine -> Text -> Text -> IO ()
append e d t = send e $ command $ Lazy.fromStrict $ "document.getElementById('" <> d <> "').innerHTML += '" <> t <> "'"

-- | Change messages that come from the JS side. Invariably, this is Text, and we choose to type convert on the haskell side
data Element = Element
  { element :: Text
  , value :: Text
  } deriving (Eq, Show, Generic)

instance ToJSON Element

instance FromJSON Element
  where
    parseJSON = withObject "Element" $ \v ->
      Element <$>
      v .: "element" <*>
      v .: "value"

main :: IO ()
main =
  scotty 3000 $ do
    middleware $ staticPolicy (noDots >-> addBase "other")
    middleware logStdoutDev
    middleware $ start $ \ ev e -> eventListener 300 ev e `E.finally`
      putStrLn ("runner finalled" :: Text)
    servePageWith "/" defaultPageConfig page1
    servePageWith "/sep" cfg2 page2
    servePageWith "/input" defaultPageConfig inputTest

