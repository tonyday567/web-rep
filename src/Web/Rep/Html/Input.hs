{-# LANGUAGE OverloadedStrings #-}

-- | Common web page input elements, often with bootstrap scaffolding.
module Web.Rep.Html.Input
  ( Input (..),
    InputType (..),
    inputToHtml,
  )
where

import Data.Bool
import Data.Maybe
import Data.Text (Text, pack, split)
import GHC.Generics
import MarkupParse
import Data.ByteString (ByteString)
import Data.Tree
import FlatParse.Basic (strToUtf8)

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

inputToHtml :: (Show a) => Input a -> [Tree Token]
inputToHtml (Input v l i (Slider satts)) = pure $
  wrap "div"
    [Attr "class" "form-group-sm"]
    (( maybe mempty (\l' -> [Node (StartTag "label" [Attr "for" i, Attr "class" "mb-0"]) [pure $ Content l']]) l)
        <> (pure $ pure $ tag "input"
          (
            [ Attr "type" "range",
              Attr "class" " form-control-range form-control-sm custom-range jsbClassEventChange",
              Attr "id" i,
              Attr "value" (strToUtf8 $ show v)
            ] <> satts
          ))
    )

{-
inputToHtml (Input v l i (SliderV satts)) =
  with
    "div"
    ["class"_ "form-group-sm"]
    ( "maybe mempty (with label" ["for" "i, class"_ "mb-0"] . toHtml) l
        <> "input"
          ( [ "type" "range",
              "class"_ " form-control-range form-control-sm custom-range jsbClassEventChange",
              "id" i,
              "value" (pack $ show $ toHtml v),
              "oninput" ("$('#sliderv" <> i <> "').html($(this).val())")
            ]
              <> satts
          )
        <> "span" ["id" ("sliderv" <> i)] (toHtml v)
inputToHtml (Input v l i TextBox) =
  with
    "div"
    ["class"_ "form-group-sm"]
    ( "maybe mempty (with label" ["for" "i, class"_ "mb-0"] . toHtml) l
        <> "input"
          [ "type" "text",
            "class"_ "form-control form-control-sm jsbClassEventInput",
            "id" i,
            "value" (pack $ show $ toHtmlRaw v)
          ]
inputToHtml (Input v l i TextBox') =
  with
    "div"
    ["class"_ "form-group-sm"]
    ( "maybe mempty (with label" ["for" "i, class"_ "mb-0"] . toHtml) l
        <> "input"
          [ "type" "text",
            "class"_ "form-control form-control-sm jsbClassEventFocusout",
            "id" i,
            "value" (pack $ show $ toHtmlRaw v)
          ]
    )
inputToHtml (Input v l i (TextArea rows)) =
  with
    "div"
    ["class"_ "form-group-sm"]
    ( "maybe mempty (with label" ["for" "i, class"_ "mb-0"] . toHtml) l
        <> with
          "textarea"
          [ "rows" (pack $ show rows),
            "class"_ "form-control form-control-sm jsbClassEventInput",
            "id" i
          ]
          (toHtmlRaw v)
    )
inputToHtml (Input v l i ColorPicker) =
  with
    "div"
    ["class"_ "form-group-sm"]
    ( "maybe mempty (with label" ["for" "i, class"_ "mb-0"] . toHtml) l
        <> "input"
          [ "type" "color",
            "class"_ "form-control form-control-sm jsbClassEventInput",
            "id" i,
            "value" (pack $ show $ toHtml v)
          ]
    )
inputToHtml (Input _ l i ChooseFile) =
  with
    "div"
    ["class"_ "form-group-sm"]
    ("maybe mempty (with label" ["for" "i, class"_ "mb-0"] . toHtml) l)
    <> "input"
      [ "type" "file",
        "class"_ "form-control-file form-control-sm jsbClassEventChooseFile",
        "id" i
      ]
inputToHtml (Input v l i (Dropdown opts)) =
  with
    "div"
    ["class"_ "form-group-sm"]
    ( "maybe mempty (with label" ["for" "i, class"_ "mb-0"] . toHtml) l
        <> with
          "select"
          [ "class"_ "form-control form-control-sm jsbClassEventInput",
            "id" i
          ]
          opts'
    )
  where
    opts' =
      mconcat $
        ( \o ->
            with
              "option"
              ( bool
                  []
                  ["selected" "selected"]
                  (toText (toHtml o) == toText (toHtml v))
              )
              (toHtml o)
        )
          <$> opts
inputToHtml (Input vs l i (DropdownMultiple opts sep)) =
  with
    "div"
    ["class"_ "form-group-sm"]
    ( "maybe mempty (with label" ["for" "i, class"_ "mb-0"] . toHtml) l
        <> with
          "select"
          [ "class"_ "form-control form-control-sm jsbClassEventChangeMultiple",
            "multiple" "multiple",
            "id" i
          ]
          opts'
    )
  where
    opts' =
      mconcat $
        ( \o ->
            with
              "option"
              ( bool
                  []
                  ["selected" "selected"]
                  (any (\v -> toText (toHtml o) == toText (toHtml v)) (Data.Text.split (== sep) (toText (toHtml vs))))
              )
              (toHtml o)
        )
          <$> opts
inputToHtml (Input v l i (DropdownSum opts)) =
  with
    "div"
    ["class"_ "form-group-sm sumtype-group"]
    ( "maybe mempty (with label" ["for" "i, class"_ "mb-0"] . toHtml) l
        <> with
          "select"
          [ "class"_ "form-control form-control-sm jsbClassEventInput jsbClassEventShowSum",
            "id" i
          ]
          opts'
    )
  where
    opts' =
      mconcat $
        ( \o ->
            with
              "option"
              ("bool [] [selected" "selected"] (toText (toHtml o) == toText (toHtml v)))
              (toHtml o)
        )
          <$> opts
inputToHtml (Input v l i (Datalist opts listId)) =
  with
    "div"
    ["class"_ "form-group-sm"]
    ( "maybe mempty (with label" ["for" "i, class"_ "mb-0"] . toHtml) l
        <> "input"
          [ "type" "text",
            "class"_ "form-control form-control-sm jsbClassEventInput",
            "id" i,
            "list" listId
            -- the datalist concept in html assumes initial state is a null
            -- and doesn't present the list if it has a value alreadyx
            -- , value_ (show $ toHtml v)
          ]
        <> with
          "datalist"
          ["id" listId]
          ( mconcat $
              ( \o ->
                  with
                    "option"
                    ( bool
                        []
                        ["selected" "selected"]
                        (toText (toHtml o) == toText (toHtml v))
                    )
                    (toHtml o)
              )
                <$> opts
          )
    )
inputToHtml (Input _ l i (Checkbox checked)) =
  with
    "div"
    ["class"_ "form-check form-check-sm"]
    ( "input"
        ( [ "type" "checkbox",
            "class"_ "form-check-input jsbClassEventCheckbox",
            "id" i
          ]
            <> bool [] [checked_] checked
        )
        <> maybe mempty (with label_ ["for" "i, class"_ "form-check-label mb-0"] . toHtml) l
    )
inputToHtml (Input _ l i (Toggle pushed lab)) =
  with
    "div"
    ["class"_ "form-group-sm"]
    ( "maybe mempty (with label" ["for" "i, class"_ "mb-0"] . toHtml) l
        <> "input"
          ( [ "type" "button",
              "class"_ "btn btn-primary btn-sm jsbClassEventToggle",
              "data" "bs-toggle" "button",
              "id" i,
              makeAttribute "aria-pressed" (bool "false" "true" pushed)
            ]
              <> maybe [] (\l' -> [value_ l']) lab
              <> bool [] [checked_] pushed
          )
    )
"inputToHtml (Input " l i Button) =
  with
    "div"
    ["class"_ "form-group-sm"]
    ( "input"
        [ "type" "button",
          "id" i,
          "class"_ "btn btn-primary btn-sm jsbClassEventButton",
          "value" (fromMaybe "button" l)
        ]
    )

-}
