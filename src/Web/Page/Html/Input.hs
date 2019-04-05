{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoPatternSynonyms #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Web.Page.Html.Input
  ( Input(Input)
  , SomeInput(..)
  , MultiInputAttributes(MultiInputAttributes)
  , InputType(..)
  , bootify
  , jsbify
  , showJsInput
  , showJs
  ) where

import Lucid
import Lucid.Base
import Data.Text
import Protolude
import Data.Generics.Product (field)
import Lens.Micro
import Web.Page.Html
import Web.Page.Types
import Web.Page.Js
import Text.InterpolatedString.Perl6

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
  Slider |
  TextBox |
  ColorPicker |
  Checkbox Bool |
  Dropdown [(Text,Text)] (Maybe Text) |
  Dropdown' [Text] (Maybe Text) |
  MultiInput (MultiInputAttributes a)
  deriving (Eq, Show, Generic)

instance ToHtml Int where
  toHtml = toHtml . (show :: Int -> Text)
  toHtmlRaw = toHtmlRaw . (show :: Int -> Text)

instance ( ) => ToHtml (InputType a) where
  toHtml Slider =
    input_ [ type_ "range"]
  toHtml TextBox =
    input_ [ type_ "text"]
  toHtml ColorPicker =
    input_ [ type_ "color"]
  toHtml (Dropdown opts mv) =
    select_ (mconcat $ (\(k,v) -> with option_ ([value_ v] <> bool [] [selected_ "selected"] (maybe False (== v) mv)) (toHtml k)) <$> opts)
  toHtml (Dropdown' opts mv) =
    select_ (mconcat $ (\v -> with option_ (bool [] [selected_ "selected"] (maybe False (== v) mv)) (toHtml v)) <$> opts)
  toHtml (Checkbox checked) =
    input_ ([type_ "checkbox"] <> bool [] [checked_] checked)
  toHtml (MultiInput (MultiInputAttributes miatts milabel)) =
    with div_ [class_ "input-group"] $
    with div_ [class_ "input-group-prepend"]
    ( with span_ [class_ "input-group-text"] $
      toHtml milabel) <>
    mconcat (toHtml <$> miatts)
  toHtmlRaw = toHtml

includeValue :: InputType a -> Bool
includeValue (Dropdown _ _) = False
includeValue (Dropdown' _ _) = False
includeValue (Checkbox _) = False
includeValue _ = True

-- the instance here needs to be `ToHtml a` because `Show a` gives "\"text\"" for show "text", and hilarity ensues
instance (Show a) => ToHtml (Input a) where
  toHtml i@(Input v itype label' wrap' idh hatts) =
    maybe identity (\c x ->  with div_ (toAtts c) x) wrap'
    (bool (l <> i') (i' <> l) (isCheckbox itype))
    where
      l = maybe mempty (with label_ ([Lucid.for_ idh] <> maybe [] (\x -> [class_ x]) (formLabelClass itype)) . toHtml) label'
      i' =
        with (toHtml itype)
        ( [id_ idh] <>
          toAtts hatts <>
          bool [] [value_ (show v)] (includeValue itype))
  toHtmlRaw (Input v itype label' wrap' idh hatts) =
    maybe identity (\c x ->  with div_ (toAtts c) x) wrap' (l <> i')
    where
      l = maybe mempty (with label_ [Lucid.for_ idh] . toHtmlRaw) label'
      i' =
        with (toHtmlRaw itype)
        ( [id_ idh] <>
          toAtts hatts <>
          bool [] [value_ (show v)] (includeValue itype))

formClass :: InputType a -> Text
formClass inp =
  case inp of
    Slider -> "form-control-range"
    (Checkbox _) -> "form-check-input"
    _ -> "form-control"

formGroupClass :: InputType a -> Text
formGroupClass inp =
  case inp of
    Slider -> "form-group-sm"
    (Checkbox _) -> "form-check"
    _ -> "form-group"

formLabelClass :: InputType a -> Maybe Text
formLabelClass inp =
  case inp of
    Slider -> Nothing
    (Checkbox _) -> Just "form-check-label"
    _ -> Nothing

isRange :: InputType a -> Bool
isRange inp =
  case inp of
    Slider -> True
    _ -> False

isCheckbox :: InputType a -> Bool
isCheckbox inp =
  case inp of
    Checkbox _ -> True
    _ -> False


bootify :: Input a -> Input a
bootify inp@(Input _ itype _ _ _ _) =
  (field @"atts" %~
   (<> [("class",
          formClass itype <>
          bool mempty " custom-range" (isRange itype)
        )])) .
  (field @"wrap" %~ (<> Just [("class", formGroupClass itype)])) $
  inp

jsbify :: Input a -> Input a
jsbify i =
  field @"atts" %~ (<>   [
    ( "oninput"
    , "jsb.event({ \"element\": this.id, \"value\": " <>
      bool "this.value" "this.checked.toString()" (isCheckbox i) <> "})"
        )]) $ i
  where
    isCheckbox (Input _ (Checkbox _) _ _ _ _) = True
    isCheckbox _ = False

showJsInput :: Text -> Text -> Input a -> Input a
showJsInput cl name = field @"atts" %~ (<> [
    ( "onchange"
    , "showJs('" <> cl <> "','" <> name <> "');"
    )])

showJs :: Page
showJs = mempty & field @"jsGlobal" .~ PageJsText
  [qc|
function showJs (cl, box) \{
  var vis = (document.getElementById(box).checked) ? "block" : "none";
  Array.from(document.getElementsByClassName(cl)).forEach(x => x.style.display = vis);
};
|]



