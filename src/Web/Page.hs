module Web.Page
  ( renderJs
  , renderCss
  , module X
  , Value(..)
  , finally
  , PixelRGB8(..)
  , HashMap.HashMap(..)
  , fromList
  ) where

import Protolude hiding (Selector)
import Web.Page.Css as X
import Web.Page.Js as X
import Web.Page.Render as X
import Web.Page.Server as X
import Web.Page.Types as X
import Web.Page.Bridge as X
import Web.Page.Rep as X
import Web.Page.Rep.Input as X
import Web.Page.Html as X
import Web.Page.Html.Input as X
import Web.Page.Bootstrap as X
import Text.InterpolatedString.Perl6 as X
import Control.Exception (finally)

import GHC.Exts (fromList)
import qualified Data.HashMap.Strict as HashMap
import qualified Web.Page.Css as Css (render)
import qualified Web.Page.Js as Js
import Data.Aeson (Value(..))
import Codec.Picture.Types (PixelRGB8(..))

renderJs :: JS -> Text
renderJs = toStrict . Js.renderToText . Js.unJS

renderCss :: Css -> Text
renderCss = toStrict . Css.render

