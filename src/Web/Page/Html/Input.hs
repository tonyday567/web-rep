{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoPatternSynonyms #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Web.Page.Html.Input
  ( Input(Input)
  , SomeInput(..)
  , MultiInputAttributes(MultiInputAttributes)
  , InputType(..)
  , bootify
  , jsbify
  ) where

import Lucid
import Lucid.Base
import Data.Text
import Protolude
import Data.Generics.Product (field)
import Lens.Micro

data Input a =
  Input
  { val :: a
  , inputType :: InputType a
  , label :: Maybe Text
  , wrap :: Maybe [(Text,Text)]
  , id' :: Text
  , atts :: [(Text, Text)]
  } deriving (Eq, Show, Generic)

-- | An existentialized 'Input'.
data SomeInput where
  SomeInput :: (a -> Html ()) -> Input a -> SomeInput

data MultiInputAttributes a =
  MultiInputAttributes
  { miAtts :: [InputType a]
  , miLabel :: Text
  }
  deriving (Eq, Show, Generic)

data InputType a =
  Range |
  TextBox |
  MultiInput (MultiInputAttributes a)
  deriving (Eq, Show, Generic)

instance ToHtml Int where
  toHtml = toHtml . (show :: Int -> Text)
  toHtmlRaw = toHtmlRaw . (show :: Int -> Text)

instance (ToHtml a) => ToHtml (InputType a) where
  toHtml Range =
    input_ [ type_ "range"]
  toHtml TextBox =
    input_ [ type_ "text"]
  toHtml (MultiInput (MultiInputAttributes miatts milabel)) =
    with div_ [class_ "input-group"] $
    with div_ [class_ "input-group-prepend"]
    ( with span_ [class_ "input-group-text"] $
      toHtml milabel) <>
    mconcat (toHtml <$> miatts)
  toHtmlRaw Range =
    input_ [ type_ "range"]
  toHtmlRaw TextBox =
    input_ [ type_ "text"]
  toHtmlRaw (MultiInput (MultiInputAttributes miatts milabel)) =
    with div_ [class_ "input-group"] $
    with div_ [class_ "input-group-prepend"]
    ( with span_ [class_ "input-group-text"] $
      toHtmlRaw milabel) <>
    mconcat (toHtmlRaw <$> miatts)


toAtts :: [(Text, Text)] -> [Attribute]
toAtts hatts = (\(t, av) -> makeAttribute t av) <$> hatts

instance (ToHtml a) => ToHtml (Input a) where
  toHtml (Input v itype label' wrap' idh hatts) =
    maybe identity (\c x ->  with div_ (toAtts c) x) wrap' (l <> i')
    where
      l = maybe mempty (with label_ [Lucid.for_ idh] . toHtml) label'
      i' =
        with (toHtml itype)
        ( [id_ idh] <>
          toAtts hatts <>
          [value_ (toStrict $ renderText $ toHtmlRaw v)])
  toHtmlRaw (Input v itype label' wrap' idh hatts) =
    maybe identity (\c x ->  with div_ (toAtts c) x) wrap' (l <> i')
    where
      l = maybe mempty (with label_ [Lucid.for_ idh] . toHtmlRaw) label'
      i' =
        with (toHtmlRaw itype)
        ( [id_ idh] <>
          toAtts hatts <>
          [value_ (toStrict $ renderText $ toHtmlRaw v)])

formClass :: Input a -> Text
formClass inp =
  case inp of
    Input _ Range _ _ _ _ -> "form-control-range"
    _ -> "form-control"

isRange :: Input a -> Bool
isRange inp =
  case inp of
    Input _ Range _ _ _ _ -> True
    _ -> False

bootify :: Input a -> Input a
bootify inp =
  (field @"atts" %~ (<> [("class", formClass inp)])) .
  (field @"atts" %~ (<> bool mempty [("class", "custom-range")] (isRange inp))) .
  (field @"wrap" %~ (<> Just [("class", "form-group")])) $
  inp

jsbify :: Input a -> Input a
jsbify =
  field @"atts" %~ (<>   [
    ( "oninput"
    , "jsb.event({ \"element\": this.id, \"value\": this.value })")])
