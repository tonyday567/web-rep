module Web.Page
  ( renderJs
  , renderCss
  , varCss
  , module X
  ) where

import Protolude hiding (Selector)
import Web.Page.Css as X
       hiding (data_, dir, main_, render, scale, scaleX, scaleY, scaleZ,
               type_, var)
import Web.Page.Html as X
import Web.Page.Js as X hiding (parse)
import Web.Page.Render as X
import Web.Page.Server as X hiding (body, method)
import Web.Page.Types as X

import qualified Web.Page.Css as Css (render, var)
import qualified Web.Page.Js as Js

renderJs :: JS -> Text
renderJs = toStrict . Js.renderToText . Js.unJS

renderCss :: Css -> Text
renderCss = toStrict . Css.render

varCss :: Selector
varCss = Css.var
