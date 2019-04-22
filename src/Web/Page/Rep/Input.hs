{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wall #-}

module Web.Page.Rep.Input
  ( repInput
  , namedRepInput
  , sliderI
  , slider
  , dropdown
  , dropdownSum
  , dropdownButton
  , colorPicker
  , textbox
  , textarea
  , checkbox
  , toggle
  , button
  , buttonB
  , maybeRep
  , repConcerns
  , viaFiddle
  ) where

import Codec.Picture.Types (PixelRGB8(..))
import Control.Category (id)
import Control.Lens
import Data.Attoparsec.Text
import Data.Biapplicative
import Data.Bifunctor (Bifunctor(..))
import Data.HashMap.Strict
import Data.Text (pack, Text)
import Lucid
import Protolude hiding ((<<*>>), Rep)
import Web.Page.Bootstrap
import Web.Page.Html
import Web.Page.Html.Input
import Web.Page.Types
import Web.Page.Rep
import Box.Cont ()

repInput :: (Monad m) => Parser a -> (a -> Text) -> Input a -> a -> SharedRepF m (Input a) a
repInput p pr i a =
  SharedRep $ do
    name <- zoom _1 genName
    zoom _2 (modify (insert name (pr a)))
    pure $
      Rep
      (bridgeify $ bootify $ set #val a $ set #id' name i)
      (\s ->
        (s, join $
        maybe (Left "lookup failed") Right $
        either (Left . (\x -> name <> ": " <> x) . pack) Right . parseOnly p <$> lookup name s))

-- | for dropdownButtons, the id needs to be injected into the InputType
repInput' :: (Monad m) => Parser a -> (a -> Text) -> (Text -> Input a) -> a -> SharedRepF m (Input a) a
repInput' p pr i a =
  SharedRep $ do
    name <- zoom _1 genName
    zoom _2 (modify (insert name (pr a)))
    pure $
      Rep
      (bridgeify $ bootify $ set #val a $ set #id' name (i name))
      (\s ->
        (s, join $
        maybe (Left "lookup failed") Right $
        either (Left . (\x -> name <> ": " <> x) . pack) Right . parseOnly p <$> lookup name s))

namedRepInput :: (MonadState (Int, HashMap Text Text) m) =>
  Parser a -> (a -> Text) -> Input a -> a -> SharedRepF m (Input a) (Text, a) 
namedRepInput p pr i a = SharedRep $ do
    name <- zoom _1 genName
    zoom _2 (modify (insert name (pr a)))
    pure $ Rep
      (bridgeify $ bootify $ set #val a $ set #id' name i)
      (\s ->
          (s, fmap (\x -> (name,x)) $
              join $
              maybe (Left "lookup failed") Right $
              either (Left . pack) Right . parseOnly p <$> lookup name s))

-- | does not put a value into the HashMap on instantiation, consumes the value when found in the HashMap, and substitutes a default on lookup failure
repMessage :: (Monad m) => Parser a -> (a -> Text) -> Input a -> a -> a -> SharedRepF m (Input a) a
repMessage p _ i def a =
  SharedRep $ do
    name <- zoom _1 genName
    pure $
      Rep
      (bridgeify $ bootify $ set #val a $ set #id' name i)
      (\s ->
        (delete name s, join $
        maybe (Right $ Right def) Right $
        either (Left . pack) Right . parseOnly p <$> lookup name s))

sliderF :: (Monad m) => Text -> Double -> Double -> Double -> Double ->
  SharedRepF m (Input Double) Double
sliderF label l u s v = repInput double show
  (Input v Slider (Just label) Nothing mempty [min_ (pack $ show l), max_ (pack $ show u), step_ (pack $ show s)]) v

slider :: (Monad m) => Text -> Double -> Double -> Double -> Double ->
  SharedRep m Double
slider label l u s v = first toHtml (sliderF label l u s v)

sliderIF :: (Monad m, Integral a, Show a) => Text -> a -> a -> a -> a ->
  SharedRepF m (Input a) a
sliderIF label l u s v = repInput decimal show 
  (Input v Slider (Just label) Nothing mempty [min_ (pack $ show l), max_ (pack $ show u), step_ (pack $ show s)]) v

sliderI :: (Monad m, ToHtml a, Integral a, Show a) => Text -> a -> a -> a -> a ->
  SharedRep m a
sliderI label l u s v = first toHtml (sliderIF label l u s v)

textboxF :: (Monad m) => Text -> Text -> SharedRepF m (Input Text) Text
textboxF label v = repInput takeText id
  (Input v TextBox (Just label) Nothing mempty []) v

textbox :: (Monad m) => Text -> Text -> SharedRep m Text
textbox label v = first toHtml (textboxF label v)

textareaF :: (Monad m) => Int -> Text -> Text -> SharedRepF m (Input Text) Text
textareaF rows label v = repInput takeText id
  (Input v (TextArea rows v) (Just label) Nothing mempty []) v

textarea :: (Monad m) => Int -> Text -> Text -> SharedRep m Text
textarea rows label v = first toHtml (textareaF rows label v)

colorPickerF :: (Monad m) => Text -> PixelRGB8 -> SharedRepF m (Input PixelRGB8) PixelRGB8
colorPickerF label v = repInput fromHex toHex
  (Input v ColorPicker (Just label) Nothing mempty []) v

colorPicker :: (Monad m) => Text -> PixelRGB8 -> SharedRep m PixelRGB8
colorPicker label v = first toHtml (colorPickerF label v)

dropdownF :: (Monad m) =>
  Parser a -> (a -> Text) -> Text -> [Text] -> a -> SharedRepF m (Input a) a
dropdownF p pr label opts v = repInput p pr 
  (Input v (Dropdown opts (Just (pr v))) (Just label) Nothing mempty []) v

dropdown :: (Monad m, ToHtml a) =>
  Parser a -> (a -> Text) -> Text -> [Text] -> a -> SharedRep m a
dropdown p pr label opts v = first toHtml (dropdownF p pr label opts v)

dropdownSumF :: (Monad m) =>
  Parser a -> (a -> Text) -> Text -> [Text] -> a -> SharedRepF m (Input a) a
dropdownSumF p pr label opts v =
  repInput p pr 
  (sumTypeShow $ Input v (Dropdown opts (Just (pr v))) (Just label) Nothing mempty []) v

dropdownSum :: (Monad m, ToHtml a) =>
  Parser a -> (a -> Text) -> Text -> [Text] -> a -> SharedRep m a
dropdownSum p pr label opts v =
  first (\x -> Lucid.with x [class__ "sumtype-group"]) $
  first toHtml (dropdownSumF p pr label opts v)

dropdownButtonF :: (Monad m) =>
  Parser a -> (a -> Text) -> Text -> [Text] -> [Text] -> a -> SharedRepF m (Input a) a
dropdownButtonF p pr label sums values v = repInput' p pr
  (\id'' -> Input v (DropdownButton sums values id'' label) (Just label) Nothing mempty []) v

dropdownButton :: (Monad m, ToHtml a) =>
  Parser a -> (a -> Text) -> Text -> [Text] -> [Text] -> a -> SharedRep m a
dropdownButton p pr label sums values v = first toHtml (dropdownButtonF p pr label sums values v)

checkboxF :: (Monad m) => Text -> Bool -> SharedRepF m (Input Bool) Bool
checkboxF label v = repInput ((=="true") <$> takeText) (bool "false" "true")
  (Input v (Checkbox v) (Just label) Nothing mempty []) v

checkbox :: (Monad m) => Text -> Bool -> SharedRep m Bool
checkbox label v = first toHtml (checkboxF label v)

toggleF :: (Monad m) => Text -> Bool -> SharedRepF m (Input Bool) Bool
toggleF label v = repInput ((=="true") <$> takeText) (bool "false" "true")
  (Input v (Toggle v label) Nothing Nothing mempty []) v

toggle :: (Monad m) => Text -> Bool -> SharedRep m Bool
toggle label v = first toHtml (toggleF label v)

buttonBF :: (Monad m) => Text -> SharedRepF m (Input Bool) Bool
buttonBF label = repMessage (pure True) (bool "false" "true")
  (Input False (Button label) Nothing Nothing mempty []) False False

buttonB :: (Monad m) => Text -> SharedRep m Bool
buttonB label = first toHtml $ buttonBF label
 
buttonF :: (Monad m) => Text -> Text -> SharedRepF m (Input Text) Text
buttonF def label = repMessage takeText show
  (Input label (Button label) Nothing Nothing mempty []) def label

button :: (Monad m) => Text -> Text -> SharedRep m Text
button def label = first toHtml $ buttonF def label

checkboxShowJs :: (Monad m) => Text -> Text -> Bool -> SharedRep m Bool
checkboxShowJs label cl v =
  SharedRep $ do
    name <- zoom _1 genName
    zoom _2 (modify (insert name (bool "false" "true" v)))
    pure $
      Rep
      ( toHtml $
        showJsInput cl name $
        bridgeify $
        bootify $
        Input v (Checkbox v) (Just label) Nothing name [])
      (\s ->
        (s, join $
        maybe (Left "lookup failed") Right $
        either (Left . pack) Right . parseOnly ((=="true") <$> takeText) <$>
        lookup name s))

maybeRep :: (Monad m) => Text -> Bool -> SharedRep m a ->
  SharedRep m (Maybe a)
maybeRep label st sa = SharedRep $ do
  className <- zoom _1 genName
  sr <- unrep $ bimap (hmap className) mmap (checkboxShowJs label className st) <<*>> sa
  pure sr
  where
    hmap cl a b =
      cardify [] a Nothing
       (Lucid.with div_
        [class__ cl, style_
                   ("display:" <> bool "none" "block" st)]
        b)
    mmap a b = bool Nothing (Just b) a

repConcerns :: (Monad m) => Concerns Text -> SharedRep m (Concerns Text, Bool)
repConcerns (Concerns c j h) =
  bimap
  (\c' j' h' up -> (Lucid.with div_ [class__ "fiddle "] $ mconcat [up,h',j',c']))
  (\c' j' h' up -> (Concerns c' j' h', up))
  (textarea 10 "css" c) <<*>>
  textarea 10 "js" j <<*>>
  textarea 10 "html" h <<*>>
  buttonB "update"

viaFiddle
  :: (Monad m)
  => SharedRep m a
  -> SharedRep m (Bool, Concerns Text, a)
viaFiddle sr = SharedRep $ do
  sr'@(Rep h _) <- unrep sr
  hrep <- unrep $ textarea 10 "html" (toText h)
  crep <- unrep $ textarea 10 "css" mempty
  jrep <- unrep $ textarea 10 "js" mempty
  u <- unrep $ buttonB "update"
  pure $
    bimap
    (\up a b c _ -> (Lucid.with div_ [class__ "fiddle "] $ mconcat [a, b, c, up]))
    (\up a b c d -> (up, Concerns a b c, d))
    u <<*>>
    crep <<*>>
    jrep <<*>>
    hrep <<*>>
    sr'

