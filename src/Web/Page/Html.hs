{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Key generators and miscellaneous html utilities.
--
-- Uses the lucid 'Lucid.Html'.
module Web.Page.Html
  ( class__,
    toText,
    genName,
    genNamePre,
    libCss,
    libJs,
    fromHex,
    toHex,
    HtmlT,
    Html,
  )
where

import Codec.Picture.Types (PixelRGB8 (..))
import Control.Monad.State
import Data.Attoparsec.Text
import Data.Bool
import Data.Text
import Data.Text.Format
import qualified Data.Text.Lazy as Lazy
import Data.Text.Lazy.Builder (toLazyText)
import Lucid
import Numeric
import Prelude

-- | FIXME: A horrible hack to separate class id's
class__ :: Text -> Attribute
class__ t = class_ (" " <> t <> " ")

-- | Convert html to text
toText :: Html a -> Text
toText = Lazy.toStrict . renderText

-- | name supply for html elements
genName :: (MonadState Int m) => m Text
genName = do
  modify (+ 1)
  (pack . show) <$> get

-- | sometimes a number doesn't work properly in html (or js???), and an alpha prefix seems to help
genNamePre :: (MonadState Int m) => Text -> m Text
genNamePre pre = (pre <>) <$> genName

-- | Convert a link to a css library from text to html.
--
-- >>> libCss "https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/css/bootstrap.min.css"
-- <link href="https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/css/bootstrap.min.css" rel="stylesheet">
libCss :: Text -> Html ()
libCss url =
  link_
    [ rel_ "stylesheet",
      href_ url
    ]

-- | Convert a link to a js library from text to html.
--
-- >>> libJs "https://code.jquery.com/jquery-3.3.1.slim.min.js"
-- <script src="https://code.jquery.com/jquery-3.3.1.slim.min.js"></script>
libJs :: Text -> Html ()
libJs url = with (script_ mempty) [src_ url]

-- | convert from #xxxxxx to 'PixelRGB8'
fromHex :: Parser PixelRGB8
fromHex =
  ( \((r, g), b) ->
      PixelRGB8 (fromIntegral r) (fromIntegral g) (fromIntegral b)
  )
    . (\(f, b) -> (f `divMod` (256 :: Int), b))
    . (`divMod` 256)
    <$> (string "#" *> hexadecimal)

-- | convert from 'PixelRGB8' to #xxxxxx
toHex :: PixelRGB8 -> Text
toHex (PixelRGB8 r g b) =
  "#"
    <> justifyRight 2 '0' (Lazy.toStrict $ toLazyText $ hex r)
    <> justifyRight 2 '0' (Lazy.toStrict $ toLazyText $ hex g)
    <> justifyRight 2 '0' (Lazy.toStrict $ toLazyText $ hex b)

-- | FIXME: `ToHtml a` is used throughout, mostly because `Show a` gives "\"text\"" for show "text", and hilarity ensues when rendering this to a web page, and I couldn't work out how to properly get around this.
--
-- Hence, these orphans.
instance ToHtml Double where

  toHtml = toHtml . (pack . show)

  toHtmlRaw = toHtmlRaw . (pack . show)

instance ToHtml Bool where

  toHtml = toHtml . bool ("false" :: Text) "true"

  toHtmlRaw = toHtmlRaw . bool ("false" :: Text) "true"

instance ToHtml Int where

  toHtml = toHtml . (pack . show)

  toHtmlRaw = toHtmlRaw . (pack . show)

instance ToHtml PixelRGB8 where

  toHtml = toHtml . toHex

  toHtmlRaw = toHtmlRaw . toHex

instance ToHtml () where

  toHtml = const "()"

  toHtmlRaw = const "()"
