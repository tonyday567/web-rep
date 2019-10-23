{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Web.Page.Html
  ( class__
  , toText
  , genName
  , genNamePre
  , libCss
  , libJs
  , fromHex
  , toHex
 ) where

-- import Data.Colour.SRGB (Colour, sRGB24show)
import Data.Text
import Lucid
import Protolude hiding ((%))
import qualified Data.Text.Lazy as Lazy
import qualified GHC.Show
import Codec.Picture.Types (PixelRGB8(..))
import Data.Attoparsec.Text
import Numeric
import Formatting hiding (string)

class__ :: Text -> Attribute
class__ t = class_ (" " <> t <> " ")

toText :: Html a -> Text
toText = Lazy.toStrict . renderText

-- | name supply for html elements
genName :: (MonadState Int m, Monad m) => m Text
genName = do
  modify (+1)
  show <$> get

-- | sometimes a number doesn't work properly in html (or js???), and an alpha prefix seems to help
genNamePre :: (MonadState Int m, Monad m) => Text -> m Text
genNamePre pre = (pre <>) <$> genName

libCss :: Text -> Html ()
libCss url = link_
  [ rel_ "stylesheet"
  , href_ url
  ]

libJs :: Text -> Html ()
libJs url = with (script_ mempty) [src_ url]

fromHex :: Parser PixelRGB8
fromHex =
  (\((r,g),b) ->
       PixelRGB8 (fromIntegral r) (fromIntegral g) (fromIntegral b)) .
    (\(f,b) -> (f `divMod` (256 :: Int), b)) .
    (`divMod` 256) <$>
    (string "#" *> hexadecimal)

toHex :: PixelRGB8 -> Text
toHex (PixelRGB8 r g b) = sformat ("#" % ((left 2 '0') %. hex) % ((left 2 '0') %. hex) % ((left 2 '0') %. hex)) r g b

-- `ToHtml a` is used throughout because `Show a` gives "\"text\"" for show "text", and hilarity ensues when rendering to the web page.
-- hence orphans
instance ToHtml Double where
  toHtml = toHtml . (show :: Double -> Text)
  toHtmlRaw = toHtmlRaw . (show :: Double -> Text)

{-
instance (RealFrac a, Floating a) => ToHtml (Colour a) where
  toHtml = toHtml . sRGB24show
  toHtmlRaw = toHtmlRaw . sRGB24show

instance (RealFrac a, Floating a) => Show (Colour a) where
  show = sRGB24show

-}

instance ToHtml Bool where
  toHtml = toHtml . bool ("false" :: Text) "true"
  toHtmlRaw = toHtmlRaw . bool ("false" :: Text) "true"

instance ToHtml Int where
  toHtml = toHtml . (show :: Int -> Text)
  toHtmlRaw = toHtmlRaw . (show :: Int -> Text)

instance ToHtml PixelRGB8 where
  toHtml = toHtml . toHex
  toHtmlRaw = toHtmlRaw . toHex

instance ToHtml () where
  toHtml = const "()"
  toHtmlRaw = const "()"

