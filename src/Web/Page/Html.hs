{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Web.Page.Html where

import Data.Colour.SRGB (Colour, sRGB24show)
import Data.Text
import Lucid
import Lucid.Base
import Protolude
import qualified Data.Text.Lazy as Lazy
import qualified GHC.Show

toAtts :: [(Text, Text)] -> [Attribute]
toAtts hatts = (\(t, av) -> makeAttribute t av) <$> hatts

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

instance ToHtml Double where
  toHtml = toHtml . (show :: Double -> Text)
  toHtmlRaw = toHtmlRaw . (show :: Double -> Text)

instance (RealFrac a, Floating a) => ToHtml (Colour a) where
  toHtml = toHtml . sRGB24show
  toHtmlRaw = toHtmlRaw . sRGB24show

instance (RealFrac a, Floating a) => Show (Colour a) where
  show = sRGB24show

instance ToHtml Bool where
  toHtml = toHtml . bool ("false" :: Text) "true"
  toHtmlRaw = toHtmlRaw . bool ("false" :: Text) "true"

