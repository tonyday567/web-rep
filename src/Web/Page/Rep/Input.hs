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
  , repMessage
  , sliderI
  , slider
  , dropdown
  , datalist
  , dropdownSum
  , colorPicker
  , textbox
  , textarea
  , checkbox
  , toggle
  , button
  , chooseFile
  , maybeRep
  , fiddle
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

-- | create a sharedRep from an Input
repInput :: (Monad m, ToHtml a, Show a) => Parser a -> (a -> Text) -> Input a -> a -> SharedRep m a
repInput p pr i a =
  SharedRep $ do
    name <- zoom _1 genName
    zoom _2 (modify (insert name (pr a)))
    pure $
      Rep
      (toHtml $ #inputVal .~ a $ #inputId .~ name $ i)
      (\s ->
        (s, join $
        maybe (Left "lookup failed") Right $
        either (Left . (\x -> name <> ": " <> x) . pack) Right . parseOnly p <$> lookup name s))

-- | does not put a value into the HashMap on instantiation, consumes the value when found in the HashMap, and substitutes a default on lookup failure
repMessage :: (Monad m, ToHtml a, Show a) => Parser a -> (a -> Text) -> Input a -> a -> a -> SharedRep m a
repMessage p _ i def a =
  SharedRep $ do
    name <- zoom _1 genName
    pure $
      Rep
      (toHtml $ #inputVal .~ a $ #inputId .~ name $ i)
      (\s ->
        (delete name s, join $
        maybe (Right $ Right def) Right $
        either (Left . pack) Right . parseOnly p <$> lookup name s))

slider :: (Monad m) => Maybe Text -> Double -> Double -> Double -> Double ->
  SharedRep m Double
slider label l u s v = repInput double show
  (Input v label mempty (Slider [min_ (pack $ show l), max_ (pack $ show u), step_ (pack $ show s)])) v

sliderI :: (Monad m, ToHtml a, Integral a, Show a) => Maybe Text -> a -> a -> a -> a ->
  SharedRep m a
sliderI label l u s v = repInput decimal show 
  (Input v label mempty (Slider [min_ (pack $ show l), max_ (pack $ show u), step_ (pack $ show s)])) v

textbox :: (Monad m) => Maybe Text -> Text -> SharedRep m Text
textbox label v = repInput takeText id
  (Input v label mempty TextBox) v

textarea :: (Monad m) => Int -> Maybe Text -> Text -> SharedRep m Text
textarea rows label v = repInput takeText id
  (Input v label mempty (TextArea rows)) v

colorPicker :: (Monad m) => Maybe Text -> PixelRGB8 -> SharedRep m PixelRGB8
colorPicker label v = repInput fromHex toHex
  (Input v label mempty ColorPicker) v

dropdown :: (Monad m, ToHtml a, Show a) =>
  Parser a -> (a -> Text) -> Maybe Text -> [Text] -> a -> SharedRep m a
dropdown p pr label opts v = repInput p pr 
  (Input v label mempty (Dropdown opts)) v

datalist :: (Monad m) => Maybe Text -> [Text] -> Text -> Text -> SharedRep m Text
datalist label opts v id'' = repInput takeText show
  (Input v label mempty (Datalist opts id'')) v

dropdownSum :: (Monad m, ToHtml a, Show a) =>
  Parser a -> (a -> Text) -> Maybe Text -> [Text] -> a -> SharedRep m a
dropdownSum p pr label opts v = repInput p pr
  (Input v label mempty (DropdownSum opts)) v

checkbox :: (Monad m) => Maybe Text -> Bool -> SharedRep m Bool
checkbox label v = repInput ((=="true") <$> takeText) (bool "false" "true")
  (Input v label mempty (Checkbox v)) v

toggle :: (Monad m) => Maybe Text -> Bool -> SharedRep m Bool
toggle label v = repInput ((=="true") <$> takeText) (bool "false" "true")
  (Input v label mempty (Toggle v label)) v

button :: (Monad m) => Maybe Text -> SharedRep m Bool
button label = repMessage (pure True) (bool "false" "true")
  (Input False label mempty Button) False False

chooseFile :: (Monad m) => Maybe Text -> Text -> SharedRep m Text
chooseFile label v = repInput takeText show
  (Input v label mempty ChooseFile) v

checkboxShowJs :: (Monad m) => Maybe Text -> Text -> Bool -> SharedRep m Bool
checkboxShowJs label cl v =
  SharedRep $ do
    name <- zoom _1 genName
    zoom _2 (modify (insert name (bool "false" "true" v)))
    pure $
      Rep
      (toHtml (Input v label name (Checkbox v)) <> scriptToggleShow name cl)
      (\s ->
        (s, join $
        maybe (Left "lookup failed") Right $
        either (Left . pack) Right . parseOnly ((=="true") <$> takeText) <$>
        lookup name s))

-- | represent a Maybe type using a checkbox hiding the underlying content on Nothing
maybeRep :: (Monad m) => Maybe Text -> Bool -> SharedRep m a ->
  SharedRep m (Maybe a)
maybeRep label st sa = SharedRep $ do
  className <- zoom _1 genName
  sr <- unrep $ bimap (hmap className) mmap (checkboxShowJs label className st) <<*>> sa
  pure sr
  where
    hmap cl a b =
      cardify (a, []) Nothing
       ((Lucid.with div_
        [class__ cl, style_
                   ("display:" <> bool "none" "block" st)]
        b),
        [style_ "padding-top: 0.25rem; padding-bottom: 0.25rem;"])
    mmap a b = bool Nothing (Just b) a

{-
maybeRepAccordion :: (Monad m) => Text -> Text -> Text -> Bool -> Maybe Text -> Bool -> SharedRep m a ->
  SharedRep m (Maybe a)
maybeRepAccordion idp idh idb collapse label st sa = SharedRep $ do
  className <- zoom _1 genName
  sr <- unrep $ bimap (hmap className) mmap (checkboxShowJs label className st) <<*>> sa
  pure sr
  where
    hmap cl a b =
      accordionCard' collapse idp idh idb
      (a, [])
      ((Lucid.with div_
        [class__ cl, style_
                   ("display:" <> bool "none" "block" st)]
        b),
       [style_ "padding-top: 0.25rem; padding-bottom: 0.25rem;"])
    mmap a b = bool Nothing (Just b) a

-}


-- | representation of web concerns (css, js & html)
fiddle :: (Monad m) => Concerns Text -> SharedRep m (Concerns Text, Bool)
fiddle (Concerns c j h) =
  bimap
  (\c' j' h' up -> (Lucid.with div_ [class__ "fiddle "] $ mconcat [up,h',j',c']))
  (\c' j' h' up -> (Concerns c' j' h', up))
  (textarea 10 (Just "css") c) <<*>>
  textarea 10 (Just "js") j <<*>>
  textarea 10 (Just "html") h <<*>>
  button (Just "update")

-- | turns a SharedRep into a fiddle
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
    (\up a b c _ -> (Lucid.with div_ [class__ "fiddle "] $ mconcat [up, a, b, c]))
    (\up a b c d -> (up, Concerns a b c, d))
    u <<*>>
    crep <<*>>
    jrep <<*>>
    hrep <<*>>
    sr'
