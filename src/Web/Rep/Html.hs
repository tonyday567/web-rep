{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Key generators and miscellaneous html utilities.
--
-- Uses the lucid 'Lucid.Html'.
module Web.Rep.Html
  ( class__,
    toText,
    toBS,
    libCss,
    libJs,
    HtmlT,
    Html,

    libCss',
    libJs',
  )
where

import Data.Bool
import Data.List (intersperse)
import Data.Text (Text, pack, unpack)
import Data.Text.Lazy qualified as Lazy
import Lucid
import Data.Markup
import Data.ByteString (ByteString)
import FlatParse.Basic


-- $setup
-- >>> :set -XOverloadedStrings

-- | FIXME: A horrible hack to separate class id's
class__ :: Text -> Attribute
class__ t = class_ (" " <> t <> " ")

-- | Convert html to text
toText :: Html a -> Text
toText = Lazy.toStrict . renderText

toBS :: Html a -> ByteString
toBS = strToUtf8 . unpack . toText

-- | Convert a link to a css library from text to html.
--
-- >>> libCss "https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/css/bootstrap.min.css"
-- <link rel="stylesheet" href="https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/css/bootstrap.min.css">
libCss :: Text -> Html ()
libCss url =
  link_
    [ rel_ "stylesheet",
      href_ url
    ]

libCss' :: ByteString -> Markup
libCss' url =
  Markup "link"
  [ ("rel", "stylesheet"),
    ("href", url)
  ]
  mempty

-- | Convert a link to a js library from text to html.
--
-- >>> libJs "https://code.jquery.com/jquery-3.3.1.slim.min.js"
-- <script src="https://code.jquery.com/jquery-3.3.1.slim.min.js"></script>
libJs :: Text -> Html ()
libJs url = with (script_ mempty) [src_ url]

libJs' :: ByteString -> Markup
libJs' url =
  Markup "script"
  [("src", url)]
  mempty

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

instance ToHtml () where
  toHtml = const "()"

  toHtmlRaw = const "()"

-- I'm going to hell for sure.
instance {-# INCOHERENT #-} (ToHtml a) => ToHtml [a] where
  toHtml = mconcat . Data.List.intersperse (toHtml ("," :: Text)) . fmap toHtml

  toHtmlRaw = mconcat . Data.List.intersperse (toHtmlRaw ("," :: Text)) . fmap toHtmlRaw
