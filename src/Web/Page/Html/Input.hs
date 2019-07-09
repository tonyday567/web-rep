{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoPatternSynonyms #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wall #-}

module Web.Page.Html.Input
  ( Input(Input)
  , Input'(Input')
  , InputType(..)
  , bootify
  , bridgeify
  , showJsInput
  , showJs
  , dbScript
  , sumTypeShow
  ) where

import Control.Category (id)
import Control.Lens
import Data.Text
import Lucid
import Lucid.Base
import Protolude hiding (for_)
import Text.InterpolatedString.Perl6
import Web.Page.Html
import Web.Page.Js
import Web.Page.Types

data Input a =
  Input
  { val :: a
  , inputType :: InputType
  , label :: Maybe Text
  , id' :: Text
  , atts :: [Attribute]
  } deriving (Eq, Show, Generic)

data Input' a =
  Input'
  { inputVal :: a
  , inputLabel :: Maybe Text
  , inputId :: Text
  , inputType' :: InputType
  } deriving (Eq, Show, Generic)

data InputType =
  Slider [Attribute] |
  TextBox |
  ColorPicker |
  Checkbox Bool |
  Toggle Bool (Maybe Text) |
  Button (Maybe Text) Text |
  Dropdown [Text] (Maybe Text) |
  DropdownSum [Text] (Maybe Text) |
  DropdownButton [Text] [Text] Text (Maybe Text) |
  TextArea Int Text |
  ChooseFile |
  Datalist [Text] (Maybe Text) Text
  deriving (Eq, Show, Generic)

instance (ToHtml a) => ToHtml (Input' a) where
  toHtml (Input' v l i (Slider satts)) =
    with div_ [class__ "form-group"]
    (input_
      ([ type_ "range"
       , class__ "islider form-control-range custom-range"
       , id_ i
       , value_ (show $ toHtml v)
       ] <> satts) <>
    maybe mempty (with label_ [for_ i] . toHtml) l) <>
    jsbScript "islider"
  toHtmlRaw = toHtml

instance ( ) => ToHtml InputType where
  toHtml (Slider _) =
    input_ [ type_ "range"]
  toHtml TextBox =
    input_ [ type_ "text"]
  toHtml ColorPicker =
    input_ [ type_ "color"]
  toHtml (Toggle pushed lab) =
    input_ ([ type_ "button", class__ "btn btn-primary btn-sm", data_ "toggle" "button",
           makeAttribute "aria-pressed" (bool "false" "true" pushed)] <> (maybe [] (\l->[value_ l]) lab))
  toHtml (Button label0 id'') =
    input_
    [ type_ "button"
    , class__ "btn btn-primary btn-sm"
    , value_ (maybe "button" id label0)
    , name_ id''
    ]
  toHtml (Dropdown opts mv) =
    select_ (mconcat $ (\v -> with option_ (bool [] [selected_ "selected"] (maybe False (== v) mv)) (toHtml v)) <$> opts)
  toHtml (DropdownSum opts mv) =
    select_ (mconcat $
             (\v -> with option_
               (bool [] [selected_ "selected"] (maybe False (== v) mv)) (toHtml v)) <$> opts)
  toHtml (DropdownButton sums values id'' label0) =
    with div_ [class__ "btn-group"]
      (button_ [ class__ "btn btn-secondary dropdown-toggle btn-select"
            , data_ "toggle" "dropdown"
            , href_ "#"
            , makeAttribute "aria-haspopup" "true"
            , makeAttribute "aria-expanded" "false"
            , id_ id''
            ] (maybe mempty toHtml label0) <>
    ul_ [ class__ "dropdown-button"
        , makeAttribute "aria-labelledby" id'']
       (mconcat $ Protolude.zipWith
        (\s v -> (with li_ [ href_ "#", data_ "value" v
                         , onclick_ ("jsb.event({ 'element': '" <> id'' <> "', 'value': '" <> s <> "'});")] (toHtml s))) sums values)) <>
    p_ [class__ "menu"] mempty <>
    dbScript
  toHtml (Checkbox checked) =
    input_ ([type_ "checkbox"] <> bool [] [checked_] checked)
  toHtml (TextArea rows v) =
    with textarea_ [rows_ (show rows)] (toHtmlRaw v)
  toHtml ChooseFile =
    input_ [ type_ "file"]
  toHtml (Datalist opts mv id'') =
    input_ [type_ "text", list_ id''] <>
    with datalist_ [id_ id''] (mconcat $ (\v -> with option_ (bool [] [selected_ "selected"] (maybe False (== v) mv)) (toHtml v)) <$> opts)
  toHtmlRaw = toHtml

includeValue :: InputType -> Bool
includeValue (Dropdown _ _) = False
includeValue DropdownButton{} = False
includeValue (Checkbox _) = False
includeValue (Toggle _ _) = False
includeValue Button{} = False
includeValue ChooseFile{} = False
includeValue _ = True

instance (ToHtml a) => ToHtml (Input a) where
  toHtml (Input v itype label0 idh hatts) =
    with div_ [class__ (formGroupClass itype)]
      (bool (l <> i') (i' <> l) (isCheckbox itype))
    where
      l = maybe mempty (with label_ ([Lucid.for_ idh] <> maybe [] (\x -> [class__ x]) (formLabelClass itype)) . toHtml) label0
      i' =
        with (toHtml itype)
        ( [id_ idh] <>
          hatts <>
          bool [] [value_ (show $ toHtmlRaw v)] (includeValue itype))
  toHtmlRaw (Input v itype label0 idh hatts) =
    (l <> i')
    where
      l = maybe mempty (with label_ [Lucid.for_ idh] . toHtmlRaw) label0
      i' =
        with (toHtmlRaw itype)
        ( [id_ idh] <>
          hatts <>
          bool [] [value_ (show $ toHtmlRaw v)] (includeValue itype))

formClass :: InputType -> Text
formClass inp =
  case inp of
    Slider{} -> "form-control-range"
    (Checkbox _) -> "form-check-input"
    (Toggle _ _) -> ""
    Button{} -> ""
    DropdownButton{} -> ""
    ChooseFile{} -> "form-control-file"
    _ -> "form-control"

formGroupClass :: InputType -> Text
formGroupClass inp =
  case inp of
    Slider{} -> "form-group-sm"
    (Checkbox _) -> "form-check"
    _ -> "form-group"

formLabelClass :: InputType -> Maybe Text
formLabelClass inp =
  case inp of
    Slider{} -> Nothing
    (Checkbox _) -> Just "form-check-label"
    _ -> Nothing

isRange :: InputType -> Bool
isRange inp =
  case inp of
    Slider{} -> True
    _ -> False

isCheckbox :: InputType -> Bool
isCheckbox inp =
  case inp of
    Checkbox _ -> True
    Toggle _ _ -> True
    Button{} -> True
    _ -> False

bootify :: Input a -> Input a
bootify inp@(Input _ itype _ _ _) =
  (#atts %~
   (<>
    [class__ (formClass itype)] <>
    bool mempty [class__ "custom-range"] (isRange itype)))
  inp

inputElement :: IsString p => InputType -> p
inputElement t =
  case t of
    Checkbox _ -> "this.checked.toString()"
    Toggle _ _ -> "(\"true\" !== this.getAttribute(\"aria-pressed\")).toString()"
    ChooseFile{} -> "this.files[0].path"
    _ -> "this.value"

bridgeify :: Input a -> Input a
bridgeify i = case i ^. #inputType of
  DropdownButton{} -> i
  _ ->
    #atts %~ (<> [
    ( funk (i ^. #inputType) $ "jsb.event({ 'element': this.id, 'value': " <>
      inputElement (i ^. #inputType)
      <> "});"
    )]) $ i
  where
    funk (Toggle _ _) = onclick_
    funk Button{} = onclick_
    funk _ = oninput_

showJsInput :: Text -> Text -> Input a -> Input a
showJsInput cl name = #atts %~ (<> [
    ( onchange_ $ "showJs('" <> cl <> "','" <> name <> "');"
    )])

showJs :: Page
showJs = mempty & #jsGlobal .~ PageJsText
  [qc|
function showJs (cl, box) \{
  var vis = (document.getElementById(box).checked) ? "block" : "none";
  Array.from(document.getElementsByClassName(cl)).forEach(x => x.style.display = vis);
};
|]

sumTypeShow :: Input a -> Input a
sumTypeShow = #atts %~ (<> [
    ( onchange_
    $ [q|var v = this.value;$(this).parent('.sumtype-group').siblings('.subtype').each(function(i) {if (this.dataset.sumtype === v) {this.style.display = 'block';} else {this.style.display = 'none';}})|]
        )])

-- https://eager.io/blog/everything-I-know-about-the-script-tag/

dbScript :: (Monad m) => HtmlT m ()
dbScript = script_ [q|
$(".dropdown-button li").click(function(){
  var selText = $(this).attr('data-value');
  $(this).parents('.btn-group').siblings('.menu').html(selText)
});
|]

jsbScript :: (Monad m) => Text -> HtmlT m ()
jsbScript cl = script_ [qq|
$('.{cl}').on('input', (function()\{
  jsb.event(\{ 'element': this.id, 'value': this.value\});
\}));
|]
