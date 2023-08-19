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
import GHC.Generics
import MarkupParse
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as C
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
inputToHtml (Input v l i (SliderV satts)) =
    pure $ wrap "div"
    [Attr "class" "form-group-sm"]
    ((maybe mempty (\l' -> [Node (StartTag "label" [Attr "for" i, Attr "class" "mb-0"]) [pure $ Content l']]) l)
        <> (pure $ pure $ tag "input"
          ( [ Attr "type" "range",
              Attr "class" " form-control-range form-control-sm custom-range jsbClassEventChange",
              Attr "id" i,
              Attr "value" (strToUtf8 $ show v),
              Attr "oninput" ("$('#sliderv" <> i <> "').html($(this).val())")
            ]
              <> satts
          ))
        <> [wrap "span" [Attr "id" ("sliderv" <> i)] [pure $ Content (strToUtf8 $ show v)]])
inputToHtml (Input v l i TextBox) =
    pure $ wrap "div"
    [Attr "class" "form-group-sm"]
    (( maybe mempty (\l' -> [wrap "label" [Attr "for" i, Attr "class" "mb-0"] [pure $ Content l']]) l)
        <> (pure $ pure $ tag "input"
          [ Attr "type" "text",
            Attr "class" "form-control form-control-sm jsbClassEventInput",
            Attr "id" i,
            Attr "value" (strToUtf8 $ show v)
          ]))
inputToHtml (Input v l i TextBox') =
    pure $ wrap "div"
    [Attr "class" "form-group-sm"]
    (( maybe mempty (\l' -> [wrap "label" [Attr "for" i, Attr "class" "mb-0"] [pure $ Content l']])l)
        <> (pure $ pure $ tag "input"
          [ Attr "type" "text",
            Attr "class" "form-control form-control-sm jsbClassEventFocusout",
            Attr "id" i,
            Attr "value" (strToUtf8 $ show v)
          ]))
inputToHtml (Input v l i (TextArea rows)) =
    pure $ wrap "div"
    [Attr "class" "form-group-sm"]
    (( maybe mempty (\l' -> [wrap "label" [Attr "for" i, Attr "class" "mb-0"] [pure $ Content l']]) l)
        <> (pure $ wrap "textarea"
          [ Attr "rows" (strToUtf8 $ show rows),
            Attr "class" "form-control form-control-sm jsbClassEventInput",
            Attr "id" i
          ]
          [pure $ Content (strToUtf8 $ show v)]))
inputToHtml (Input v l i ColorPicker) =
    pure $ wrap "div"
    [Attr "class" "form-group-sm"]
    (( maybe mempty (\l' -> [wrap "label" [Attr "for" i, Attr "class" "mb-0"] [pure $ Content l']]) l)
        <> (pure $ pure $ tag "input"
          [ Attr "type" "color",
            Attr "class" "form-control form-control-sm jsbClassEventInput",
            Attr "id" i,
            Attr "value" (strToUtf8 $ show v)
          ]
    ))
inputToHtml (Input _ l i ChooseFile) =
    pure $ wrap "div"
    [Attr "class" "form-group-sm"]
    (( maybe mempty (\l' -> [wrap "label" [Attr "for" i, Attr "class" "mb-0"] [pure $ Content l']]) l)
        <> (pure $ pure $ tag "input"
      [ Attr "type" "file",
        Attr "class" "form-control-file form-control-sm jsbClassEventChooseFile",
        Attr "id" i
      ]))
inputToHtml (Input v l i (Dropdown opts)) =
    pure $ wrap "div"
    [Attr "class" "form-group-sm"]
    (( maybe mempty (\l' -> [wrap "label" [Attr "for" i, Attr "class" "mb-0"] [pure $ Content l']]) l)
        <> (pure $ wrap "select"
          [ Attr "class" "form-control form-control-sm jsbClassEventInput",
            Attr "id" i
          ]
          opts'
    ))
  where
    opts' =
        ( \o ->
              wrap "option"
              ( bool
                  []
                  [Attr "selected" "selected"]
                  (o==(strToUtf8 $ show v))
              )
              [pure $ Content o])
          <$> opts
inputToHtml (Input vs l i (DropdownMultiple opts sep)) =
    pure $ wrap "div"
    [Attr "class" "form-group-sm"]
    (( maybe mempty (\l' -> [wrap "label" [Attr "for" i, Attr "class" "mb-0"] [pure $ Content l']]) l)
        <> (pure $ wrap "select"
          [ Attr "class" "form-control form-control-sm jsbClassEventChangeMultiple",
            Attr "multiple" "multiple",
            Attr "id" i
          ]
          opts'
    ))
  where
    opts' =
        ( \o ->
            wrap "option"
              ( bool
                  []
                  [Attr "selected" "selected"]
                  (any (\v -> o==(strToUtf8 $ show v)) (C.split sep (strToUtf8 $ show vs)))
              )
              [pure (Content o)])
          <$> opts
inputToHtml (Input v l i (DropdownSum opts)) =
    pure $ wrap "div"
    [Attr "class" "form-group-sm sumtype-group"]
    (( maybe mempty (\l' -> [wrap "label" [Attr "for" i, Attr "class" "mb-0"] [pure $ Content l']]) l)
        <> (pure $ wrap "select"
          [ Attr "class" "form-control form-control-sm jsbClassEventInput jsbClassEventShowSum",
            Attr "id" i
          ]
          opts'
    ))
  where
    opts' =
        ( \o ->
              wrap "option"
              (bool [] [Attr "selected" "selected"] (o==(strToUtf8 $ show v)))
              [pure (Content o)])
          <$> opts
inputToHtml (Input v l i (Datalist opts listId)) =
    pure $ wrap "div"
    [Attr "class" "form-group-sm"]
    (( maybe mempty (\l' -> [wrap "label" [Attr "for" i, Attr "class" "mb-0"] [pure $ Content l']]) l)
        <> (pure $ pure $ tag "input"
          [ Attr "type" "text",
            Attr "class" "form-control form-control-sm jsbClassEventInput",
            Attr "id" i,
            Attr "list" listId
            -- the datalist concept in html assumes initial state is a null
            -- and doesn't present the list if it has a value alreadyx
            -- , value_ (show $ toHtml v)
          ])
        <> [wrap "datalist"
          [Attr "id" listId]
           (   ( \o ->
                  wrap "option"
                    ( bool
                        []
                        [Attr "selected" "selected"]
                        (o==(strToUtf8 $ show v))
                    )
                    [pure $ Content o])
                <$> opts)
          ]
    )
inputToHtml (Input _ l i (Checkbox checked)) =
    pure $ wrap "div"
    [Attr "class" "form-check form-check-sm"]
    ( pure $ wrap "input"
        ( [ Attr "type" "checkbox",
            Attr "class" "form-check-input jsbClassEventCheckbox",
            Attr "id" i
          ]
            <> bool [] [Attr "checked" ""] checked
        )
    (( maybe mempty (\l' -> [wrap "label" [Attr "for" i, Attr "class" "form-label-check mb-0"] [pure $ Content l']]) l)
    ))
inputToHtml (Input _ l i (Toggle pushed lab)) =
    pure $ wrap "div"
    [Attr "class" "form-group-sm"]
    (( maybe mempty (\l' -> [wrap "label" [Attr "for" i, Attr "class" "mb-0"] [pure $ Content l']]) l)
        <> (pure $ pure $ tag "input"
          ( [ Attr "type" "button",
              Attr "class" "btn btn-primary btn-sm jsbClassEventToggle",
              Attr "data-bs-toggle" "button",
              Attr "id" i,
              Attr "aria-pressed" (bool "false" "true" pushed)
            ]
              <> maybe [] (\l' -> [Attr "value" l']) lab
              <> bool [] [Attr "checked" ""] pushed
          ))
    )
inputToHtml (Input _ l i Button) =
    pure $ wrap "div"
    [Attr "class" "form-group-sm"]
    ( pure $ pure $ tag "input"
        [ Attr "type" "button",
          Attr "id" i,
          Attr "class" "btn btn-primary btn-sm jsbClassEventButton",
          Attr "value" (fromMaybe "button" l)
        ]
    )

