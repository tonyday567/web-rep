{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wall #-}

module Web.Page.Html.Input
  ( Input(Input)
  , InputType(..)
  , scriptToggleShow
  ) where

import Data.Text
import Lucid
import Lucid.Base
import Protolude hiding (for_)
import Text.InterpolatedString.Perl6
import Web.Page.Html

-- | something that might exist on a web page and be a front-end input to computations.
data Input a =
  Input
  { inputVal :: a
  , inputLabel :: Maybe Text
  , inputId :: Text
  , inputType :: InputType
  } deriving (Eq, Show, Generic)

-- | various types of Inputs, that encapsulate practical bootstrap class functionality
data InputType =
  Slider [Attribute] |
  TextBox |
  TextArea Int |
  ColorPicker |
  ChooseFile |
  Dropdown [Text] |
  DropdownSum [Text] |
  Datalist [Text] Text |
  Checkbox Bool |
  Toggle Bool (Maybe Text) |
  Button
  deriving (Eq, Show, Generic)

instance (ToHtml a) => ToHtml (Input a) where
  toHtml (Input v l i (Slider satts)) =
    with div_ [class__ "form-group"]
    ((maybe mempty (with label_ [for_ i] . toHtml) l) <>
     input_
      ([ type_ "range"
       , class__ " form-control-range custom-range"
       , id_ i
       , value_ (show $ toHtml v)
       ] <> satts) <>
      scriptJsbEvent i "change")
  toHtml (Input v l i TextBox) =
    with div_ [class__ "form-group"]
    ((maybe mempty (with label_ [for_ i] . toHtml) l) <>
    input_
      ([ type_ "text"
       , class__ "form-control"
       , id_ i
       , value_ (show $ toHtml v)
       ]) <>
      scriptJsbEvent i "input")
  toHtml (Input v l i (TextArea rows)) =
    with div_ [class__ "form-group"]
    (maybe mempty (with label_ [for_ i] . toHtml) l <>
     (with textarea_
     [ rows_ (show rows)
     , class__ "form-control"
     , id_ i
     ] (toHtmlRaw v)
    ) <>
     scriptJsbEvent i "input")
  toHtml (Input v l i ColorPicker) =
    with div_ [class__ "form-group"]
    ((maybe mempty (with label_ [for_ i] . toHtml) l) <>
    input_
      ([ type_ "color"
       , class__ "form-control"
       , id_ i
       , value_ (show $ toHtml v)
       ]) <>
     scriptJsbEvent i "input")
  toHtml (Input _ l i ChooseFile) =
    with div_ [class__ "form-group"]
    (maybe mempty (with label_ [for_ i] . toHtml) l) <>
     input_
      ([ type_ "file"
       , class__ "form-control-file"
       , id_ i
       ]) <>
    scriptJsbChooseFile i
  toHtml (Input v l i (Dropdown opts)) =
    with div_ [class__ "form-group"]
    ((maybe mempty (with label_ [for_ i] . toHtml) l) <>
    (with select_
      [ class__ "form-control"
      , id_ i
      ] opts') <>
     scriptJsbEvent i "input")
    where
      opts' = mconcat $
        (\o -> with option_ (bool [] [selected_ "selected"]
                            (toText (toHtml o) == toText (toHtml v)))
               (toHtml o)) <$> opts
  toHtml (Input v l i (DropdownSum opts)) =
    with div_ [class__ "form-group sumtype-group"]
    ((maybe mempty (with label_ [for_ i] . toHtml) l) <>
    (with select_
      [ class__ "form-control"
      , id_ i
      ] opts') <>
    scriptShowSum i <>
     scriptJsbEvent i "input")
    where
      opts' = mconcat $
        (\o -> with option_
          (bool [] [selected_ "selected"] (toText (toHtml o) == toText (toHtml v)))
               (toHtml o)) <$> opts
  toHtml (Input v l i (Datalist opts listId)) =
    with div_ [class__ "form-group"]
    ((maybe mempty (with label_ [for_ i] . toHtml) l) <>
    input_
      [ type_ "text"
       , class__ "form-control"
       , id_ i
       , list_ listId
       -- the datalist concept in html assumes initial state is a null
       -- and doesn't present the list if it has a value alreadyx
       -- , value_ (show $ toHtml v)
       ] <>
     with datalist_ [id_ listId] (mconcat $ (\o ->
       with option_ (bool [] [selected_ "selected"]
                     (toText (toHtml o) == toText (toHtml v)))
       (toHtml o)) <$> opts) <>
     scriptJsbEvent i "input")
  -- FIXME: How can you refactor to eliminate this polymorphic wart?
  toHtml (Input _ l i (Checkbox checked)) =
    with div_ [class__ "form-check"]
    (input_
      ([ type_ "checkbox"
       , class__ "form-check-input"
       , id_ i
       ] <>
       bool [] [checked_] checked) <>
      (maybe mempty (with label_ [for_ i, class__ "form-check-label"] . toHtml) l) <>
    scriptJsbCheckbox i)
  toHtml (Input _ l i (Toggle pushed lab)) =
    with div_ [class__ "form-group"]
    (( maybe mempty (with label_ [for_ i] . toHtml) l) <>
    input_
     ([ type_ "button"
      , class__ "btn btn-primary btn-sm"
      , data_ "toggle" "button"
      , id_ i
      , makeAttribute "aria-pressed" (bool "false" "true" pushed)
      ] <>
      (maybe [] (\l' -> [value_ l']) lab) <>
      bool [] [checked_] pushed) <>
      scriptJsbToggle i)
  toHtml (Input _ l i Button) =
    with div_ [class__ "form-group"]
    (input_
     ( [ type_ "button"
       , id_ i
       , class__ "btn btn-primary btn-sm"
       , value_ (fromMaybe "button" l)
       ]) <>
    scriptJsbButton i)

  toHtmlRaw = toHtml

-- scripts attached to Inputs
-- https://eager.io/blog/everything-I-know-about-the-script-tag/

scriptJsbEvent :: (Monad m) => Text -> Text -> HtmlT m ()
scriptJsbEvent name event = script_ [qq|
$('#{name}').on('{event}', (function()\{
  jsb.event(\{ 'element': this.id, 'value': this.value\});
\}));
|]

scriptJsbButton :: (Monad m) => Text -> HtmlT m ()
scriptJsbButton name = script_ [qq|
$('#{name}').on('click', (function()\{
  jsb.event(\{ 'element': this.id, 'value': this.value\});
\}));
|]

scriptJsbToggle :: (Monad m) => Text -> HtmlT m ()
scriptJsbToggle name = script_ [qq|
$('#{name}').on('click', (function()\{
  jsb.event(\{ 'element': this.id, 'value': (\"true\" !== this.getAttribute(\"aria-pressed\")).toString()\});
\}));
|]

scriptJsbCheckbox :: (Monad m) => Text -> HtmlT m ()
scriptJsbCheckbox name = script_ [qq|
$('#{name}').on('click', (function()\{
  jsb.event(\{ 'element': this.id, 'value': this.checked.toString()\});
\}));
|]

scriptJsbChooseFile :: (Monad m) => Text -> HtmlT m ()
scriptJsbChooseFile name = script_ [qq|
$('#{name}').on('input', (function()\{
  jsb.event(\{ 'element': this.id, 'value': this.files[0].name\});
\}));
|]

scriptShowSum :: (Monad m) => Text -> HtmlT m ()
scriptShowSum name = script_ [qq|
$('#{name}').on('change', (function()\{
  var v = this.value;
  $(this).parent('.sumtype-group').siblings('.subtype').each(function(i) \{
    if (this.dataset.sumtype === v) \{
      this.style.display = 'block';
      \} else \{
      this.style.display = 'none';
      \}\})
  \}));
|]

scriptToggleShow :: (Monad m) => Text -> Text -> HtmlT m ()
scriptToggleShow checkName toggleClass = script_ [qq|
$('#{checkName}').on('change', (function()\{
  var vis = this.checked ? "block" : "none";
  Array.from(document.getElementsByClassName({toggleClass})).forEach(x => x.style.display = vis);
\}));
|]
