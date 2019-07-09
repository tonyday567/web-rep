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
  , repInput'
  , sliderI
  , slider
  , slider'
  , dropdown
  , datalist
  , dropdownSum
  , dropdownButton
  , colorPicker
  , textbox
  , textarea
  , checkbox
  , toggle
  , button
  , chooseFile
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

repInput :: (Monad m, ToHtml a) => Parser a -> (a -> Text) -> (Text -> Input a) -> a -> SharedRep m a
repInput p pr i a =
  SharedRep $ do
    name <- zoom _1 genName
    zoom _2 (modify (insert name (pr a)))
    pure $
      Rep
      (toHtml $ bridgeify $ bootify $ set #val a (i name))
      (\s ->
        (s, join $
        maybe (Left "lookup failed") Right $
        either (Left . (\x -> name <> ": " <> x) . pack) Right . parseOnly p <$> lookup name s))

repInput' :: (Monad m, ToHtml a) => Parser a -> (a -> Text) -> Input' a -> a -> SharedRep m a
repInput' p pr i a =
  SharedRep $ do
    name <- zoom _1 genName
    zoom _2 (modify (insert name (pr a)))
    pure $
      Rep
      (toHtml $ #inputVal .~ a $ #inputVal .~ name $ i)
      (\s ->
        (s, join $
        maybe (Left "lookup failed") Right $
        either (Left . (\x -> name <> ": " <> x) . pack) Right . parseOnly p <$> lookup name s))


-- | does not put a value into the HashMap on instantiation, consumes the value when found in the HashMap, and substitutes a default on lookup failure
repMessage :: (Monad m, ToHtml a) => Parser a -> (a -> Text) -> (Text -> Input a) -> a -> a -> SharedRep m a
repMessage p _ i def a =
  SharedRep $ do
    name <- zoom _1 genName
    pure $
      Rep
      (toHtml $ bridgeify $ bootify $ set #val a $ i name)
      (\s ->
        (delete name s, join $
        maybe (Right $ Right def) Right $
        either (Left . pack) Right . parseOnly p <$> lookup name s))

slider :: (Monad m) => Maybe Text -> Double -> Double -> Double -> Double ->
  SharedRep m Double
slider label l u s v = first toHtml $ repInput double show
  (\i -> Input v (Slider [min_ (pack $ show l), max_ (pack $ show u), step_ (pack $ show s)]) label i []) v

slider' :: (Monad m) => Maybe Text -> Double -> Double -> Double -> Double ->
  SharedRep m Double
slider' label l u s v = repInput' double show
  (Input' v label mempty (Slider [min_ (pack $ show l), max_ (pack $ show u), step_ (pack $ show s)])) v

sliderI :: (Monad m, ToHtml a, Integral a, Show a) => Maybe Text -> a -> a -> a -> a ->
  SharedRep m a
sliderI label l u s v = repInput decimal show 
  (\i -> Input v (Slider [min_ (pack $ show l), max_ (pack $ show u), step_ (pack $ show s)]) label i []) v

textbox :: (Monad m) => Maybe Text -> Text -> SharedRep m Text
textbox label v = repInput takeText id
  (\i -> Input v TextBox label i []) v

textarea :: (Monad m) => Int -> Maybe Text -> Text -> SharedRep m Text
textarea rows label v = repInput takeText id
  (\i -> Input v (TextArea rows v) label i []) v

colorPicker :: (Monad m) => Maybe Text -> PixelRGB8 -> SharedRep m PixelRGB8
colorPicker label v = repInput fromHex toHex
  (\i -> Input v ColorPicker label i []) v

dropdown :: (Monad m, ToHtml a) =>
  Parser a -> (a -> Text) -> Maybe Text -> [Text] -> a -> SharedRep m a
dropdown p pr label opts v = repInput p pr 
  (\i -> Input v (Dropdown opts (Just (pr v))) label i []) v

datalist :: (Monad m) => Maybe Text -> [Text] -> Text -> SharedRep m Text
datalist label opts v = repInput takeText show
  (\i -> Input v (Datalist opts (Just v) i) label i []) v

dropdownSum :: (Monad m, ToHtml a) =>
  Parser a -> (a -> Text) -> Maybe Text -> [Text] -> a -> SharedRep m a
dropdownSum p pr label opts v = first (\x -> Lucid.with x [class__ "sumtype-group"]) $
  repInput p pr
  (\i -> sumTypeShow $ Input v (Dropdown opts (Just (pr v))) label i []) v

dropdownButton :: (Monad m, ToHtml a) =>
  Parser a -> (a -> Text) -> Maybe Text -> [Text] -> [Text] -> a -> SharedRep m a
dropdownButton p pr label sums values v = repInput p pr
  (\i -> Input v (DropdownButton sums values i label) label i []) v

checkbox :: (Monad m) => Maybe Text -> Bool -> SharedRep m Bool
checkbox label v = repInput ((=="true") <$> takeText) (bool "false" "true")
  (\i -> Input v (Checkbox v) label i []) v

toggle :: (Monad m) => Maybe Text -> Bool -> SharedRep m Bool
toggle label v = repInput ((=="true") <$> takeText) (bool "false" "true")
  (\i -> Input v (Toggle v label) Nothing i []) v

button :: (Monad m) => Maybe Text -> SharedRep m Bool
button label = repMessage (pure True) (bool "false" "true")
  (\i -> Input False (Button label i) Nothing i []) False False

chooseFile :: (Monad m) => Maybe Text -> Text -> SharedRep m Text
chooseFile label v = repInput takeText show
  (\i -> Input v ChooseFile label i []) v

checkboxShowJs :: (Monad m) => Maybe Text -> Text -> Bool -> SharedRep m Bool
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
        Input v (Checkbox v) label name [])
      (\s ->
        (s, join $
        maybe (Left "lookup failed") Right $
        either (Left . pack) Right . parseOnly ((=="true") <$> takeText) <$>
        lookup name s))

maybeRep :: (Monad m) => Maybe Text -> Bool -> SharedRep m a ->
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
  (textarea 10 (Just "css") c) <<*>>
  textarea 10 (Just "js") j <<*>>
  textarea 10 (Just "html") h <<*>>
  button (Just "update")

viaFiddle
  :: (Monad m)
  => SharedRep m a
  -> SharedRep m (Bool, Concerns Text, a)
viaFiddle sr = SharedRep $ do
  sr'@(Rep h _) <- unrep sr
  hrep <- unrep $ textarea 10 (Just "html") (toText h)
  crep <- unrep $ textarea 10 (Just "css") mempty
  jrep <- unrep $ textarea 10 (Just "js") mempty
  u <- unrep $ button (Just "update")
  pure $
    bimap
    (\up a b c _ -> (Lucid.with div_ [class__ "fiddle "] $ mconcat [a, b, c, up]))
    (\up a b c d -> (up, Concerns a b c, d))
    u <<*>>
    crep <<*>>
    jrep <<*>>
    hrep <<*>>
    sr'
