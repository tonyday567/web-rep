module Lucid.Page
  ( renderJs
  , renderCss
  , var
  , varCss
  , module X
  ) where

import qualified Data.Text.Lazy as Lazy
import           Lucid.Page.Css as X hiding (render, var, scale, scaleX, scaleY, scaleZ, dir, type_, main_, data_)
import           Lucid.Page.Js as X hiding (render, var, renderJs)
import           Lucid.Page.Render as X
import           Lucid.Page.Server as X hiding (body, method)
import           Lucid.Page.Html as X
import           Lucid.Page.Types as X

import qualified Lucid.Page.Css as Css (render, var)
import qualified Lucid.Page.Js as Js (render, var)

renderJs :: JStat -> String
renderJs = Js.render

renderCss :: Css -> Lazy.Text
renderCss = Css.render

var :: String -> JStat
var = Js.var

varCss :: Selector
varCss = Css.var
