-- | haskell for web page representations
module Web.Page
  ( module Web.Page.Types,
    module Web.Page.SharedReps,
    module Web.Page.Render,
    module Web.Page.Server,
    module Web.Page.Bridge,
    module Web.Page.Html,
    module Web.Page.Html.Input,
    module Web.Page.Bootstrap,
    module Web.Page.Mathjax,
    Value (..),
    finally,
    PixelRGB8 (..),
    HashMap.HashMap (..),
    fromList,
    void,
    sequenceA_,
    Text,
    pack,
    unpack,
    bool,
    module X,
  )
where

import Codec.Picture.Types (PixelRGB8 (..))
import Control.Applicative as X
import Control.Exception (finally)
import Control.Monad
import Control.Monad.Trans.State as X
import Data.Aeson (Value (..))
import Data.Biapplicative as X
import Data.Bifunctor as X
import Data.Bool
import Data.Foldable (sequenceA_)
import qualified Data.HashMap.Strict as HashMap
import Data.Text (Text, pack, unpack)
import Data.Text.Lazy (toStrict)
import GHC.Exts (fromList)
import Text.InterpolatedString.Perl6 as X
import Web.Page.Bootstrap
import Web.Page.Bridge
import Web.Page.Html
import Web.Page.Html.Input
import Web.Page.Mathjax
import Web.Page.Render
import Web.Page.Server
import Web.Page.SharedReps
import Web.Page.Types
