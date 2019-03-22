module Web.Page
  ( renderJs
  , renderCss
  , module X
  ) where

import Protolude hiding (Selector)
import Web.Page.Css as X
import Web.Page.Js as X
import Web.Page.Render as X
import Web.Page.Server as X
import Web.Page.Types as X

import qualified Web.Page.Css as Css (render)
import qualified Web.Page.Js as Js

renderJs :: JS -> Text
renderJs = toStrict . Js.renderToText . Js.unJS

renderCss :: Css -> Text
renderCss = toStrict . Css.render

