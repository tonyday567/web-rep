{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

-- | Key generators and miscellaneous html utilities.
--
module Web.Rep.Html
  ( libCss,
    libJs,
  )
where

import MarkupParse
import Data.ByteString (ByteString)
import Data.Tree

-- $setup
--
-- >>> import Web.Rep.Html
-- >>> :set -XOverloadedStrings

-- | Convert a link to a css library from text to html.
--
-- >>> libCss "https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/css/bootstrap.min.css"
-- <link rel="stylesheet" href="https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/css/bootstrap.min.css">
libCss :: ByteString -> Tree Token
libCss url = pure $
  tag "link"
    [ Attr "rel" "stylesheet",
      Attr "href" url
    ]

-- | Convert a link to a js library from text to html.
--
-- >>> libJs "https://code.jquery.com/jquery-3.3.1.slim.min.js"
-- <script src="https://code.jquery.com/jquery-3.3.1.slim.min.js"></script>
libJs :: ByteString -> Tree Token
libJs url = pure $ tag "script" [Attr "src" url]
