{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Common web page input elements, often with bootstrap scaffolding.
module Web.Rep.Html.Input
  ( Input (..),
    InputType (..),
    markupInput,
    ToByteString (..),
  )
where

import Data.Bool
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as C
import Data.Maybe
import Data.Text (Text)
import Data.Text.Encoding
import GHC.Generics
import MarkupParse

-- | Conversion to a 'ByteString'
class ToByteString a where
  -- | Convert a value to a strict ByteString
  toByteString :: a -> ByteString
  default toByteString :: (Show a) => a -> ByteString
  toByteString = strToUtf8 . show

instance ToByteString ByteString where
  toByteString = id

instance ToByteString Text where
  toByteString = encodeUtf8

instance ToByteString Int

instance ToByteString Integer

instance ToByteString Double

instance ToByteString Float

instance ToByteString Bool

-- | something that might exist on a web page and be a front-end input to computations.
data Input a = Input
  { -- | underlying value
    inputVal :: a,
    -- | label suggestion
    inputLabel :: Maybe ByteString,
    -- | name//key//id of the Input
    inputId :: ByteString,
    -- | type of html input
    inputType :: InputType
  }
  deriving (Eq, Show, Generic)

-- | Various types of web page inputs, encapsulating practical bootstrap class functionality
data InputType
  = Slider [Attr]
  | SliderV [Attr]
  | TextBox
  | TextBox'
  | TextArea Int
  | ColorPicker
  | ChooseFile
  | Dropdown [ByteString]
  | DropdownMultiple [ByteString] Char
  | DropdownSum [ByteString]
  | Datalist [ByteString] ByteString
  | Checkbox Bool
  | Toggle Bool (Maybe ByteString)
  | Button
  deriving (Eq, Show, Generic)

-- | Convert an 'Input' to 'Markup' via a specific printer.
markupInput :: (a -> ByteString) -> Input a -> Markup
markupInput pr (Input v l i (Slider satts)) =
  element
    "div"
    [Attr "class" "form-group-sm"]
    (maybe mempty (elementc "label" [Attr "for" i, Attr "class" "mb-0"]) l)
    <> element_
      "input"
      ( [ Attr "type" "range",
          Attr "class" " form-control-range form-control-sm custom-range jsbClassEventChange",
          Attr "id" i,
          Attr "value" (pr v)
        ]
          <> satts
      )
markupInput pr (Input v l i (SliderV satts)) =
  element
    "div"
    [Attr "class" "form-group-sm"]
    ( maybe mempty (elementc "label" [Attr "for" i, Attr "class" "mb-0"]) l
        <> element_
          "input"
          ( [ Attr "type" "range",
              Attr "class" " form-control-range form-control-sm custom-range jsbClassEventChange",
              Attr "id" i,
              Attr "value" (pr v),
              Attr "oninput" ("$('#sliderv" <> i <> "').html($(this).val())")
            ]
              <> satts
          )
    )
    <> elementc "span" [Attr "id" ("sliderv" <> i)] (pr v)
markupInput pr (Input v l i TextBox) =
  element
    "div"
    [Attr "class" "form-group-sm"]
    ( maybe mempty (elementc "label" [Attr "for" i, Attr "class" "mb-0"]) l
        <> element_
          "input"
          [ Attr "type" "text",
            Attr "class" "form-control form-control-sm jsbClassEventInput",
            Attr "id" i,
            Attr "value" (pr v)
          ]
    )
markupInput pr (Input v l i TextBox') =
  element
    "div"
    [Attr "class" "form-group-sm"]
    ( maybe mempty (elementc "label" [Attr "for" i, Attr "class" "mb-0"]) l
        <> element_
          "input"
          [ Attr "type" "text",
            Attr "class" "form-control form-control-sm jsbClassEventFocusout",
            Attr "id" i,
            Attr "value" (pr v)
          ]
    )
markupInput pr (Input v l i (TextArea rows)) =
  element
    "div"
    [Attr "class" "form-group-sm"]
    ( maybe mempty (elementc "label" [Attr "for" i, Attr "class" "mb-0"]) l
        <> elementc
          "textarea"
          [ Attr "rows" (toByteString rows),
            Attr "class" "form-control form-control-sm jsbClassEventInput",
            Attr "id" i
          ]
          (pr v)
    )
markupInput pr (Input v l i ColorPicker) =
  element
    "div"
    [Attr "class" "form-group-sm"]
    ( maybe mempty (elementc "label" [Attr "for" i, Attr "class" "mb-0"]) l
        <> element_
          "input"
          [ Attr "type" "color",
            Attr "class" "form-control form-control-sm jsbClassEventInput",
            Attr "id" i,
            Attr "value" (pr v)
          ]
    )
markupInput _ (Input _ l i ChooseFile) =
  element
    "div"
    [Attr "class" "form-group-sm"]
    ( maybe mempty (elementc "label" [Attr "for" i, Attr "class" "mb-0"]) l
        <> element_
          "input"
          [ Attr "type" "file",
            Attr "class" "form-control-file form-control-sm jsbClassEventChooseFile",
            Attr "id" i
          ]
    )
markupInput pr (Input v l i (Dropdown opts)) =
  element
    "div"
    [Attr "class" "form-group-sm"]
    ( maybe mempty (elementc "label" [Attr "for" i, Attr "class" "mb-0"]) l
        <> element
          "select"
          [ Attr "class" "form-control form-control-sm jsbClassEventInput",
            Attr "id" i
          ]
          (mconcat opts')
    )
  where
    opts' =
      ( \o ->
          elementc
            "option"
            ( bool
                []
                [Attr "selected" "selected"]
                (o == pr v)
            )
            o
      )
        <$> opts
markupInput pr (Input vs l i (DropdownMultiple opts sep)) =
  element
    "div"
    [Attr "class" "form-group-sm"]
    ( maybe mempty (elementc "label" [Attr "for" i, Attr "class" "mb-0"]) l
        <> element
          "select"
          [ Attr "class" "form-control form-control-sm jsbClassEventChangeMultiple",
            Attr "multiple" "multiple",
            Attr "id" i
          ]
          (mconcat opts')
    )
  where
    opts' =
      ( \o ->
          elementc
            "option"
            ( bool
                []
                [Attr "selected" "selected"]
                (any (\v -> o == strToUtf8 (show v)) (C.split sep (pr vs)))
            )
            o
      )
        <$> opts
markupInput pr (Input v l i (DropdownSum opts)) =
  element
    "div"
    [Attr "class" "form-group-sm sumtype-group"]
    ( maybe mempty (elementc "label" [Attr "for" i, Attr "class" "mb-0"]) l
        <> element
          "select"
          [ Attr "class" "form-control form-control-sm jsbClassEventInput jsbClassEventShowSum",
            Attr "id" i
          ]
          (mconcat opts')
    )
  where
    opts' =
      ( \o ->
          elementc
            "option"
            (bool [] [Attr "selected" "selected"] (o == pr v))
            o
      )
        <$> opts
markupInput pr (Input v l i (Datalist opts listId)) =
  element
    "div"
    [Attr "class" "form-group-sm"]
    ( maybe mempty (elementc "label" [Attr "for" i, Attr "class" "mb-0"]) l
        <> element_
          "input"
          [ Attr "type" "text",
            Attr "class" "form-control form-control-sm jsbClassEventInput",
            Attr "id" i,
            Attr "list" listId
            -- the datalist concept in html assumes initial state is a null
            -- and doesn't present the list if it has a value alreadyx
            -- , value_ (show $ toHtml v)
          ]
        <> element
          "datalist"
          [Attr "id" listId]
          ( mconcat
              ( ( \o ->
                    elementc
                      "option"
                      ( bool
                          []
                          [Attr "selected" "selected"]
                          (o == pr v)
                      )
                      o
                )
                  <$> opts
              )
          )
    )
markupInput _ (Input _ l i (Checkbox checked)) =
  element
    "div"
    [Attr "class" "form-check form-check-sm"]
    ( element
        "input"
        ( [ Attr "type" "checkbox",
            Attr "class" "form-check-input jsbClassEventCheckbox",
            Attr "id" i
          ]
            <> bool [] [Attr "checked" ""] checked
        )
        ( maybe mempty (elementc "label" [Attr "for" i, Attr "class" "form-label-check mb-0"]) l
        )
    )
markupInput _ (Input _ l i (Toggle pushed lab)) =
  element
    "div"
    [Attr "class" "form-group-sm"]
    ( maybe mempty (elementc "label" [Attr "for" i, Attr "class" "mb-0"]) l
        <> element_
          "input"
          ( [ Attr "type" "button",
              Attr "class" "btn btn-primary btn-sm jsbClassEventToggle",
              Attr "data-bs-toggle" "button",
              Attr "id" i,
              Attr "aria-pressed" (bool "false" "true" pushed)
            ]
              <> maybe [] (\l' -> [Attr "value" l']) lab
              <> bool [] [Attr "checked" ""] pushed
          )
    )
markupInput _ (Input _ l i Button) =
  element
    "div"
    [Attr "class" "form-group-sm"]
    ( element_
        "input"
        [ Attr "type" "button",
          Attr "id" i,
          Attr "class" "btn btn-primary btn-sm jsbClassEventButton",
          Attr "value" (fromMaybe "button" l)
        ]
    )
