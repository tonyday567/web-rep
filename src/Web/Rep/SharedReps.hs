{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wall #-}

-- | Various SharedRep instances for common html input elements.
module Web.Rep.SharedReps
  ( repInput,
    repMessage,
    sliderI,
    slider,
    dropdown,
    dropdownMultiple,
    datalist,
    dropdownSum,
    colorPicker,
    textbox,
    textarea,
    checkbox,
    toggle,
    button,
    chooseFile,
    maybeRep,
    fiddle,
    viaFiddle,
    accordionList,
    listMaybeRep,
    listRep,
    readTextbox,
    defaultListLabels,
    repChoice,
    subtype,
    selectItems,
    repItemsSelect,
  )
where

import Box.Codensity ()
import Control.Monad
import Control.Monad.State.Lazy
import Data.Attoparsec.Text hiding (take)
import qualified Data.Attoparsec.Text as A
import Data.Biapplicative
import Data.Bool
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List as List
import Data.Text (Text, intercalate, pack, unpack)
import Lucid
import Optics.Core
import Optics.Zoom
import Text.InterpolatedString.Perl6
import Web.Rep.Bootstrap
import Web.Rep.Html
import Web.Rep.Html.Input
import Web.Rep.Page
import Web.Rep.Shared
import Prelude as P

-- $setup
-- >>> :set -XOverloadedStrings

-- | Create a sharedRep from an Input.
repInput ::
  (Monad m, ToHtml a) =>
  -- | Parser
  Parser a ->
  -- | Printer
  (a -> Text) ->
  -- | 'Input' type
  Input a ->
  -- | initial value
  a ->
  SharedRep m a
repInput p pr i = register (first pack . A.parseOnly p) pr (\n v -> toHtml $ #inputVal .~ v $ #inputId .~ n $ i)

-- | Like 'repInput', but does not put a value into the HashMap on instantiation, consumes the value when found in the HashMap, and substitutes a default on lookup failure
repMessage :: (Monad m, ToHtml a) => Parser a -> (a -> Text) -> Input a -> a -> a -> SharedRep m a
repMessage p _ i def a =
  message (first pack . A.parseOnly p) (\n v -> toHtml $ #inputVal .~ v $ #inputId .~ n $ i) a def

-- | double slider
--
-- For Example, a slider between 0 and 1 with a step of 0.01 and a default value of 0.3 is:
--
-- > :t slider (Just "label") 0 1 0.01 0.3
-- slider (Just "label") 0 1 0.01 0.3 :: Monad m => SharedRep m Double
slider ::
  (Monad m) =>
  Maybe Text ->
  Double ->
  Double ->
  Double ->
  Double ->
  SharedRep m Double
slider label l u s v =
  repInput
    double
    (pack . show)
    (Input v label mempty (Slider [min_ (pack $ show l), max_ (pack $ show u), step_ (pack $ show s)]))
    v

-- | integral slider
--
-- For Example, a slider between 0 and 1000 with a step of 10 and a default value of 300 is:
--
-- > :t sliderI (Just "label") 0 1000 10 300
-- sliderI (Just "label") 0 1000 10 300
--   :: (Monad m, ToHtml a, P.Integral a, Show a) => SharedRep m a
sliderI ::
  (Monad m, ToHtml a, P.Integral a, Show a) =>
  Maybe Text ->
  a ->
  a ->
  a ->
  a ->
  SharedRep m a
sliderI label l u s v =
  repInput
    decimal
    (pack . show)
    (Input v label mempty (Slider [min_ (pack $ show l), max_ (pack $ show u), step_ (pack $ show s)]))
    v

-- | textbox classique
--
-- > :t textbox (Just "label") "some text"
-- textbox (Just "label") "some text" :: Monad m => SharedRep m Text
textbox :: (Monad m) => Maybe Text -> Text -> SharedRep m Text
textbox label v =
  repInput
    takeText
    id
    (Input v label mempty TextBox)
    v

-- | textbox that only updates on focusout
textbox' :: (Monad m) => Maybe Text -> Text -> SharedRep m Text
textbox' label v =
  repInput
    takeText
    id
    (Input v label mempty TextBox')
    v

-- | textarea input element, specifying number of rows.
textarea :: (Monad m) => Int -> Maybe Text -> Text -> SharedRep m Text
textarea rows label v =
  repInput
    takeText
    id
    (Input v label mempty (TextArea rows))
    v

-- | Non-typed hex color input
colorPicker :: (Monad m) => Maybe Text -> Text -> SharedRep m Text
colorPicker label v =
  repInput
    takeText
    id
    (Input v label mempty ColorPicker)
    v

-- | dropdown box
dropdown ::
  (Monad m, ToHtml a) =>
  -- | parse an a from Text
  Parser a ->
  -- | print an a to Text
  (a -> Text) ->
  -- | label suggestion
  Maybe Text ->
  -- | list of dropbox elements (as text)
  [Text] ->
  -- | initial value
  a ->
  SharedRep m a
dropdown p pr label opts v =
  repInput
    p
    pr
    (Input v label mempty (Dropdown opts))
    v

-- | dropdown box with multiple selections
dropdownMultiple ::
  (Monad m, ToHtml a) =>
  -- | parse an a from Text
  Parser a ->
  -- | print an a to Text
  (a -> Text) ->
  -- | label suggestion
  Maybe Text ->
  -- | list of dropbox elements (as text)
  [Text] ->
  -- | initial value
  [a] ->
  SharedRep m [a]
dropdownMultiple p pr label opts vs =
  repInput
    (p `sepBy1` char ',')
    (intercalate "," . fmap pr)
    (Input vs label mempty (DropdownMultiple opts ','))
    vs

-- | a datalist input
datalist :: (Monad m) => Maybe Text -> [Text] -> Text -> Text -> SharedRep m Text
datalist label opts v id'' =
  repInput
    takeText
    (pack . show)
    (Input v label mempty (Datalist opts id''))
    v

-- | A dropdown box designed to help represent a haskell sum type.
dropdownSum ::
  (Monad m, ToHtml a) =>
  Parser a ->
  (a -> Text) ->
  Maybe Text ->
  [Text] ->
  a ->
  SharedRep m a
dropdownSum p pr label opts v =
  repInput
    p
    pr
    (Input v label mempty (DropdownSum opts))
    v

-- | A checkbox input.
checkbox :: (Monad m) => Maybe Text -> Bool -> SharedRep m Bool
checkbox label v =
  repInput
    ((== "true") <$> takeText)
    (bool "false" "true")
    (Input v label mempty (Checkbox v))
    v

-- | a toggle button
toggle :: (Monad m) => Maybe Text -> Bool -> SharedRep m Bool
toggle label v =
  repInput
    ((== "true") <$> takeText)
    (bool "false" "true")
    (Input v label mempty (Toggle v label))
    v

-- | a button
button :: (Monad m) => Maybe Text -> SharedRep m Bool
button label =
  repMessage
    (pure True)
    (bool "false" "true")
    (Input False label mempty Button)
    False
    False

-- | filename input
chooseFile :: (Monad m) => Maybe Text -> Text -> SharedRep m Text
chooseFile label v =
  repInput
    takeText
    (pack . show)
    (Input v label mempty ChooseFile)
    v

-- | Represent a Maybe using a checkbox.
--
-- Hides the underlying content on Nothing
maybeRep ::
  (Monad m) =>
  Maybe Text ->
  Bool ->
  SharedRep m a ->
  SharedRep m (Maybe a)
maybeRep label st sa = SharedRep $ do
  id' <- zoom _1 (genNamePre "maybe")
  unshare $ bimap (hmap id') mmap (checkboxShow label id' st) <<*>> sa
  where
    hmap id' a b =
      cardify
        (a, [])
        Nothing
        ( Lucid.with
            div_
            [ id_ id',
              style_
                ("display:" <> bool "none" "block" st)
            ]
            b,
          [style_ "padding-top: 0.25rem; padding-bottom: 0.25rem;"]
        )
    mmap a b = bool Nothing (Just b) a

checkboxShow :: (Monad m) => Maybe Text -> Text -> Bool -> SharedRep m Bool
checkboxShow label id' v =
  SharedRep $ do
    name <- zoom _1 genName
    zoom _2 (modify (HashMap.insert name (bool "false" "true" v)))
    pure $
      Rep
        (toHtml (Input v label name (Checkbox v)) <> scriptToggleShow name id')
        ( \s ->
            ( s,
              join $
                maybe (Left "HashMap.lookup failed") Right $
                  either (Left . pack) Right . parseOnly ((== "true") <$> takeText)
                    <$> HashMap.lookup name s
            )
        )

-- | toggle show/hide
scriptToggleShow :: (Monad m) => Text -> Text -> HtmlT m ()
scriptToggleShow checkName toggleId =
  script_
    [qq|
$('#{checkName}').on('change', (function()\{
  var vis = this.checked ? "block" : "none";
  document.getElementById("{toggleId}").style.display = vis;
\}));
|]

-- | A (fixed-size) list represented in html as an accordion card
-- A major restriction of the library is that a 'SharedRep' does not have a Monad instance. In practice, this means that the external representation of lists cannot have a dynamic size.
accordionList :: (Monad m) => Maybe Text -> Text -> Maybe Text -> (Text -> a -> SharedRep m a) -> [Text] -> [a] -> SharedRep m [a]
accordionList title prefix open srf labels as = SharedRep $ do
  (Rep h fa) <-
    unshare $
      first (accordion prefix open . zip labels) $
        foldr
          (\a x -> bimap (:) (:) a <<*>> x)
          (pure [])
          (zipWith srf labels as)
  h' <- zoom _1 h
  pure (Rep (maybe mempty (h5_ . toHtml) title <> h') fa)

-- | A (fixed-sized) list of (Bool, a) tuples.
accordionBoolList :: (Monad m) => Maybe Text -> Text -> (a -> SharedRep m a) -> (Bool -> SharedRep m Bool) -> [Text] -> [(Bool, a)] -> SharedRep m [(Bool, a)]
accordionBoolList title prefix bodyf checkf labels xs = SharedRep $ do
  (Rep h fa) <-
    unshare $
      first
        ( accordionChecked prefix
            . zipWith (\l (ch, a) -> (l, a, ch)) labels
        )
        ( foldr
            (\a x -> bimap (:) (:) a <<*>> x)
            (pure [])
            ((\(ch, a) -> bimap (,) (,) (checkf ch) <<*>> bodyf a) <$> xs)
        )
  h' <- zoom _1 h
  pure (Rep (maybe mempty (h5_ . toHtml) title <> h') fa)

-- | A fixed-sized list of Maybe a\'s
listMaybeRep :: (Monad m) => Maybe Text -> Text -> (Text -> Maybe a -> SharedRep m (Maybe a)) -> Int -> [a] -> SharedRep m [Maybe a]
listMaybeRep t p srf n as =
  accordionList t p Nothing srf (defaultListLabels n) (take n ((Just <$> as) <> repeat Nothing))

-- | A SharedRep of [a].  Due to the applicative nature of the bridge, the size of lists has to be fixed on construction.  listRep is a workaround for this, to enable some form of dynamic sizing.
listRep ::
  (Monad m) =>
  Maybe Text ->
  Text ->
  -- | name prefix (should be unique)
  (Bool -> SharedRep m Bool) ->
  -- | Bool Rep
  (a -> SharedRep m a) ->
  -- | a Rep
  Int ->
  -- | maximum length of list
  a ->
  -- | default value for new rows
  [a] ->
  -- | initial values
  SharedRep m [a]
listRep t p brf srf n defa as =
  second (mconcat . fmap (\(b, a) -> bool [] [a] b)) $
    accordionBoolList
      t
      p
      srf
      brf
      (defaultListLabels n)
      (take n (((True,) <$> as) <> repeat (False, defa)))

-- a sensible default for the accordion row labels for a list
defaultListLabels :: Int -> [Text]
defaultListLabels n = (\x -> "[" <> pack (show x) <> "]") <$> [0 .. n] :: [Text]

-- | Parse from a textbox
--
-- Uses focusout so as not to spam the reader.
readTextbox :: (Monad m, Read a, Show a) => Maybe Text -> a -> SharedRep m (Either Text a)
readTextbox label v = parsed . unpack <$> textbox' label (pack $ show v)
  where
    parsed str =
      case reads str of
        [(a, "")] -> Right a
        _badRead -> Left (pack str)

-- | Representation of web concerns (css, js & html).
fiddle :: (Monad m) => Concerns Text -> SharedRep m (Concerns Text, Bool)
fiddle (Concerns c j h) =
  bimap
    (\c' j' h' up -> Lucid.with div_ [class__ "fiddle "] $ mconcat [up, h', j', c'])
    (\c' j' h' up -> (Concerns c' j' h', up))
    (textarea 10 (Just "css") c)
    <<*>> textarea 10 (Just "js") j
    <<*>> textarea 10 (Just "html") h
    <<*>> button (Just "update")

-- | turns a SharedRep into a fiddle
viaFiddle ::
  (Monad m) =>
  SharedRep m a ->
  SharedRep m (Bool, Concerns Text, a)
viaFiddle sr = SharedRep $ do
  sr'@(Rep h _) <- unshare sr
  hrep <- unshare $ textarea 10 (Just "html") (toText h)
  crep <- unshare $ textarea 10 (Just "css") mempty
  jrep <- unshare $ textarea 10 (Just "js") mempty
  u <- unshare $ button (Just "update")
  pure $
    bimap
      (\up a b c _ -> Lucid.with div_ [class__ "fiddle "] $ mconcat [up, a, b, c])
      (\up a b c d -> (up, Concerns a b c, d))
      u
      <<*>> crep
      <<*>> jrep
      <<*>> hrep
      <<*>> sr'

repChoice :: (Monad m) => Int -> [(Text, SharedRep m a)] -> SharedRep m a
repChoice initt xs =
  bimap hmap mmap dd
    <<*>> foldr (\x a -> bimap (:) (:) x <<*>> a) (pure []) cs
  where
    ts = fst <$> xs
    cs = snd <$> xs
    dd = dropdownSum takeText id Nothing ts t0
    t0 = ts List.!! initt
    hmap dd' cs' =
      div_
        ( dd'
            <> mconcat (zipWith (`subtype` t0) cs' ts)
        )
    mmap dd' cs' = maybe (List.head cs') (cs' List.!!) (List.elemIndex dd' ts)

-- | select test keys from a Map
selectItems :: [Text] -> HashMap.HashMap Text a -> [(Text, a)]
selectItems ks m =
  HashMap.toList $
    HashMap.filterWithKey (\k _ -> k `elem` ks) m

-- | rep of multiple items list
repItemsSelect :: Monad m => [Text] -> [Text] -> SharedRep m [Text]
repItemsSelect initial full =
  dropdownMultiple (A.takeWhile (`notElem` ([','] :: [Char]))) id (Just "items") full initial

subtype :: With a => a -> Text -> Text -> a
subtype h origt t =
  with
    h
    [ class__ "subtype ",
      data_ "sumtype" t,
      style_ ("display:" <> bool "block" "none" (origt /= t))
    ]
