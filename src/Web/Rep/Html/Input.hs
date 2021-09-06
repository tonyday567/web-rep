{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wall #-}

-- | Common web page input elements, often with bootstrap scaffolding.
module Web.Rep.Html.Input
  ( Input (..),
    InputType (..),
  )
where

import Data.Text (split)
import Lucid
import Lucid.Base
import Web.Rep.Html
import Data.Text (Text, pack)
import Data.Bool
import GHC.Generics
import Data.Maybe

-- | something that might exist on a web page and be a front-end input to computations.
data Input a = Input
  { -- | underlying value
    inputVal :: a,
    -- | label suggestion
    inputLabel :: Maybe Text,
    -- | name//key//id of the Input
    inputId :: Text,
    -- | type of html input
    inputType :: InputType
  }
  deriving (Eq, Show, Generic)

-- | Various types of web page inputs, encapsulating practical bootstrap class functionality
data InputType
  = Slider [Attribute]
  | TextBox
  | TextBox'
  | TextArea Int
  | ColorPicker
  | ChooseFile
  | Dropdown [Text]
  | DropdownMultiple [Text] Char
  | DropdownSum [Text]
  | Datalist [Text] Text
  | Checkbox Bool
  | Toggle Bool (Maybe Text)
  | Button
  deriving (Eq, Show, Generic)

instance (ToHtml a) => ToHtml (Input a) where
  toHtml (Input v l i (Slider satts)) =
    with
      div_
      [class__ "form-group-sm"]
      ( maybe mempty (with label_ [for_ i, class__ "mb-0"] . toHtml) l
          <> input_
            ( [ type_ "range",
                class__ " form-control-range form-control-sm custom-range jsbClassEventChange",
                id_ i,
                value_ (pack $ show $ toHtml v)
              ]
                <> satts
            )
      )
  toHtml (Input v l i TextBox) =
    with
      div_
      [class__ "form-group-sm"]
      ( maybe mempty (with label_ [for_ i, class__ "mb-0"] . toHtml) l
          <> input_
            [ type_ "text",
              class__ "form-control form-control-sm jsbClassEventInput",
              id_ i,
              value_ (pack $ show $ toHtmlRaw v)
            ]
      )
  toHtml (Input v l i TextBox') =
    with
      div_
      [class__ "form-group-sm"]
      ( maybe mempty (with label_ [for_ i, class__ "mb-0"] . toHtml) l
          <> input_
            [ type_ "text",
              class__ "form-control form-control-sm jsbClassEventFocusout",
              id_ i,
              value_ (pack $ show $ toHtmlRaw v)
            ]
      )
  toHtml (Input v l i (TextArea rows)) =
    with
      div_
      [class__ "form-group-sm"]
      ( maybe mempty (with label_ [for_ i, class__ "mb-0"] . toHtml) l
          <> with
            textarea_
            [ rows_ (pack $ show rows),
              class__ "form-control form-control-sm jsbClassEventInput",
              id_ i
            ]
            (toHtmlRaw v)
      )
  toHtml (Input v l i ColorPicker) =
    with
      div_
      [class__ "form-group-sm"]
      ( maybe mempty (with label_ [for_ i, class__ "mb-0"] . toHtml) l
          <> input_
            [ type_ "color",
              class__ "form-control form-control-sm jsbClassEventInput",
              id_ i,
              value_ (pack $ show $ toHtml v)
            ]
      )
  toHtml (Input _ l i ChooseFile) =
    with
      div_
      [class__ "form-group-sm"]
      (maybe mempty (with label_ [for_ i, class__ "mb-0"] . toHtml) l)
      <> input_
        [ type_ "file",
          class__ "form-control-file form-control-sm jsbClassEventChooseFile",
          id_ i
        ]
  toHtml (Input v l i (Dropdown opts)) =
    with
      div_
      [class__ "form-group-sm"]
      ( maybe mempty (with label_ [for_ i, class__ "mb-0"] . toHtml) l
          <> with
            select_
            [ class__ "form-control form-control-sm jsbClassEventInput",
              id_ i
            ]
            opts'
      )
    where
      opts' =
        mconcat $
          ( \o ->
              with
                option_
                ( bool
                    []
                    [selected_ "selected"]
                    (toText (toHtml o) == toText (toHtml v))
                )
                (toHtml o)
          )
            <$> opts
  toHtml (Input vs l i (DropdownMultiple opts sep)) =
    with
      div_
      [class__ "form-group-sm"]
      ( maybe mempty (with label_ [for_ i, class__ "mb-0"] . toHtml) l
          <> with
            select_
            [ class__ "form-control form-control-sm jsbClassEventChangeMultiple",
              multiple_ "multiple",
              id_ i
            ]
            opts'
      )
    where
      opts' =
        mconcat $
          ( \o ->
              with
                option_
                ( bool
                    []
                    [selected_ "selected"]
                    (any (\v -> toText (toHtml o) == toText (toHtml v)) (Data.Text.split (== sep) (toText (toHtml vs))))
                )
                (toHtml o)
          )
            <$> opts
  toHtml (Input v l i (DropdownSum opts)) =
    with
      div_
      [class__ "form-group-sm sumtype-group"]
      ( maybe mempty (with label_ [for_ i, class__ "mb-0"] . toHtml) l
          <> with
            select_
            [ class__ "form-control form-control-sm jsbClassEventInput jsbClassEventShowSum",
              id_ i
            ]
            opts'
      )
    where
      opts' =
        mconcat $
          ( \o ->
              with
                option_
                (bool [] [selected_ "selected"] (toText (toHtml o) == toText (toHtml v)))
                (toHtml o)
          )
            <$> opts
  toHtml (Input v l i (Datalist opts listId)) =
    with
      div_
      [class__ "form-group-sm"]
      ( maybe mempty (with label_ [for_ i, class__ "mb-0"] . toHtml) l
          <> input_
            [ type_ "text",
              class__ "form-control form-control-sm jsbClassEventInput",
              id_ i,
              list_ listId
              -- the datalist concept in html assumes initial state is a null
              -- and doesn't present the list if it has a value alreadyx
              -- , value_ (show $ toHtml v)
            ]
          <> with
            datalist_
            [id_ listId]
            ( mconcat $
                ( \o ->
                    with
                      option_
                      ( bool
                          []
                          [selected_ "selected"]
                          (toText (toHtml o) == toText (toHtml v))
                      )
                      (toHtml o)
                )
                  <$> opts
            )
      )
  -- FIXME: How can you refactor to eliminate this polymorphic wart?
  toHtml (Input _ l i (Checkbox checked)) =
    with
      div_
      [class__ "form-check form-check-sm"]
      ( input_
          ( [ type_ "checkbox",
              class__ "form-check-input jsbClassEventCheckbox",
              id_ i
            ]
              <> bool [] [checked_] checked
          )
          <> maybe mempty (with label_ [for_ i, class__ "form-check-label mb-0"] . toHtml) l
      )
  toHtml (Input _ l i (Toggle pushed lab)) =
    with
      div_
      [class__ "form-group-sm"]
      ( maybe mempty (with label_ [for_ i, class__ "mb-0"] . toHtml) l
          <> input_
            ( [ type_ "button",
                class__ "btn btn-primary btn-sm jsbClassEventToggle",
                data_ "toggle" "button",
                id_ i,
                makeAttribute "aria-pressed" (bool "false" "true" pushed)
              ]
                <> maybe [] (\l' -> [value_ l']) lab
                <> bool [] [checked_] pushed
            )
      )
  toHtml (Input _ l i Button) =
    with
      div_
      [class__ "form-group-sm"]
      ( input_
          [ type_ "button",
            id_ i,
            class__ "btn btn-primary btn-sm jsbClassEventButton",
            value_ (fromMaybe "button" l)
          ]
      )

  toHtmlRaw = toHtml
