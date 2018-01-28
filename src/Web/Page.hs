module Web.Page
  ( renderJs
  , renderCss
  , var
  , varCss
  , module X
  ) where

import qualified Data.Text.Lazy as Lazy
import           Web.Page.Css as X hiding (render, var, scale, scaleX, scaleY, scaleZ, dir, type_, main_, data_)
import           Web.Page.Js as X hiding (render, var, renderJs)
import           Web.Page.Render as X
import           Web.Page.Server as X hiding (body, method)
import           Web.Page.Html as X
import           Web.Page.Types as X

import qualified Web.Page.Css as Css (render, var)
import qualified Web.Page.Js as Js (render, var)

renderJs :: JStat -> String
renderJs = Js.render

renderCss :: Css -> Lazy.Text
renderCss = Css.render

var :: String -> JStat
var = Js.var

varCss :: Selector
varCss = Css.var
