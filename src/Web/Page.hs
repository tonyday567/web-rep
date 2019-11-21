
-- | haskell for web page representations

module Web.Page
  ( module X
  , Value(..)
  , finally
  , PixelRGB8(..)
  , HashMap.HashMap(..)
  , fromList
  , void
  , sequenceA_
  , Text
  , pack
  , unpack
  , bool
  ) where

import Web.Page.SharedReps as X
import Web.Page.Render as X
import Web.Page.Server as X
import Web.Page.Types as X
import Web.Page.Bridge as X
import Web.Page.Html as X
import Web.Page.Html.Input as X
import Web.Page.Bootstrap as X
import Text.InterpolatedString.Perl6 as X
import Control.Monad.Trans.State as X
import Data.Bifunctor as X
import Control.Applicative as X
import Data.Biapplicative as X
import Control.Exception (finally)
import Data.Text.Lazy (toStrict)
import GHC.Exts (fromList)
import qualified Data.HashMap.Strict as HashMap
import Data.Aeson (Value(..))
import Codec.Picture.Types (PixelRGB8(..))
import Control.Monad
import Data.Foldable (sequenceA_)
import Data.Text (Text, pack, unpack)
import Data.Bool
