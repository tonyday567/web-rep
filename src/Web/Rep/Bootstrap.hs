{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- | Some <https://getbootstrap.com/ bootstrap> assets and functionality.
module Web.Rep.Bootstrap
  ( bootstrapCss',
    bootstrapJs',
    bootstrapMeta',
    bootstrapPage,
    cardify,
    divClass_,
    accordion,
    accordionChecked,
    accordionCard,
    accordionCardChecked,
    accordion_,
  )
where

import Control.Monad.State.Lazy
import Data.Bool
import Data.Functor.Identity
import Data.Markup
import Data.Text (Text)
import Lucid
import Lucid.Base
import Web.Rep.Html
import Web.Rep.Page
import Web.Rep.Shared
import Data.ByteString (ByteString)

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Web.Rep
-- >>> import Data.Markup

-- | <link href="https://cdn.jsdelivr.net/npm/bootstrap@5.0.2/dist/css/bootstrap.min.css" rel="stylesheet" integrity="sha384-EVSTQN3/azprG1Anm3QDgpJLIm9Nao0Yz1ztcQTwFspd3yD65VohhpuuCOmLASjC" crossorigin="anonymous">
bootstrapCss :: [Html ()]
bootstrapCss =
  [ link_
      [ rel_ "stylesheet",
        href_ "https://cdn.jsdelivr.net/npm/bootstrap@5.0.2/dist/css/bootstrap.min.css",
        integrity_ "sha384-EVSTQN3/azprG1Anm3QDgpJLIm9Nao0Yz1ztcQTwFspd3yD65VohhpuuCOmLASjC",
        crossorigin_ "anonymous"
      ]
  ]
-- | Bootstrap 5 CSS link
--
-- >>> encodeMarkup bootstrapCss'
-- "<link rel=\"stylesheet\" href=\"https://cdn.jsdelivr.net/npm/bootstrap@5.0.2/dist/css/bootstrap.min.css\" integrity=\"sha384-EVSTQN3/azprG1Anm3QDgpJLIm9Nao0Yz1ztcQTwFspd3yD65VohhpuuCOmLASjC\" crossorigin=\"anonymous\">"
bootstrapCss' :: Markup
bootstrapCss' =
  Markup "link"
      [ ( "rel", "stylesheet" ),
        ( "href", "https://cdn.jsdelivr.net/npm/bootstrap@5.0.2/dist/css/bootstrap.min.css" ),
        ( "integrity", "sha384-EVSTQN3/azprG1Anm3QDgpJLIm9Nao0Yz1ztcQTwFspd3yD65VohhpuuCOmLASjC" ),
        ( "crossorigin", "anonymous" )
      ]
      mempty

-- | <script src="https://cdn.jsdelivr.net/npm/bootstrap@5.0.2/dist/js/bootstrap.bundle.min.js" integrity="sha384-MrcW6ZMFYlzcLA8Nl+NtUVF0sA7MsXsP1UyJoMp4YLEuNSfAP+JcXn/tWtIaxVXM" crossorigin="anonymous"></script>
bootstrapJs :: [Html ()]
bootstrapJs =
  [ with
      (script_ mempty)
      [ src_ "https://cdn.jsdelivr.net/npm/bootstrap@5.0.2/dist/js/bootstrap.bundle.min.js",
        integrity_ "sha384-MrcW6ZMFYlzcLA8Nl+NtUVF0sA7MsXsP1UyJoMp4YLEuNSfAP+JcXn/tWtIaxVXM",
        crossorigin_ "anonymous"
      ],
    with
      (script_ mempty)
      [ src_ "https://code.jquery.com/jquery-3.3.1.slim.min.js",
        integrity_ "sha384-q8i/X+965DzO0rT7abK41JStQIAqVgRVzpbzo5smXKp4YfRvH+8abtTE1Pi6jizo",
        crossorigin_ "anonymous"
      ]
  ]

-- <link rel="stylesheet" href="https://cdn.jsdelivr.net/npm/bootstrap@5.0.2/dist/css/bootstrap.min.css" integrity="sha384-EVSTQN3/azprG1Anm3QDgpJLIm9Nao0Yz1ztcQTwFspd3yD65VohhpuuCOmLASjC" crossorigin="anonymous"><meta charset="utf-8"><meta name="viewport" content="width=device-width, initial-scale=1, shrink-to-fit=no"></head><body><script src="https://cdn.jsdelivr.net/npm/bootstrap@5.0.2/dist/js/bootstrap.bundle.min.js" integrity="sha384-MrcW6ZMFYlzcLA8Nl+NtUVF0sA7MsXsP1UyJoMp4YLEuNSfAP+JcXn/tWtIaxVXM" crossorigin="anonymous"></script><script src="https://code.jquery.com/jquery-3.3.1.slim.min.js" integrity="sha384-q8i/X+965DzO0rT7abK41JStQIAqVgRVzpbzo5smXKp4YfRvH+8abtTE1Pi6jizo" crossorigin="anonymous"><

bootstrapJs' :: [ Markup ]
bootstrapJs' =
  [ Markup "script"
      [ ( "src", "https://cdn.jsdelivr.net/npm/bootstrap@5.0.2/dist/js/bootstrap.bundle.min.js" ),
        ("integrity", "sha384-MrcW6ZMFYlzcLA8Nl+NtUVF0sA7MsXsP1UyJoMp4YLEuNSfAP+JcXn/tWtIaxVXM"),
        ( "crossorigin", "anonymous" )
      ] mempty,
    Markup "script"
      [ ("src", "https://code.jquery.com/jquery-3.3.1.slim.min.js"),
        ("integrity", "sha384-q8i/X+965DzO0rT7abK41JStQIAqVgRVzpbzo5smXKp4YfRvH+8abtTE1Pi6jizo"),
        ("crossorigin", "anonymous")
      ] mempty
  ]

bootstrapMeta :: [Html ()]
bootstrapMeta =
  [ meta_ [charset_ "utf-8"],
    meta_
      [ name_ "viewport",
        content_ "width=device-width, initial-scale=1, shrink-to-fit=no"
      ]
  ]

bootstrapMeta' :: [ Markup ]
bootstrapMeta' =
  [ Markup "meta" [("charset", "utf-8")] mempty,
    Markup "meta"
      [ ("name", "viewport"),
        ("content", "width=device-width, initial-scale=1, shrink-to-fit=no")
      ] mempty
  ]

-- | A page containing all the <https://getbootstrap.com/ bootstrap> needs for a web page.
bootstrapPage :: Page
bootstrapPage =
  Page
    bootstrapCss
    bootstrapJs
    mempty
    mempty
    mempty
    (mconcat bootstrapMeta)
    mempty

-- | wrap some Html with the bootstrap <https://getbootstrap.com/docs/4.3/components/card/ card> class
cardify :: (Html (), [Attribute]) -> Maybe Text -> (Html (), [Attribute]) -> Html ()
cardify (h, hatts) t (b, batts) =
  with div_ ([class__ "card"] <> hatts) $
    h
      <> with
        div_
        ([class__ "card-body"] <> batts)
        ( maybe mempty (with h5_ [class__ "card-title"] . toHtml) t
            <> b
        )

cardify' :: ([Content], Attributes) -> Maybe ByteString -> ([Content], Attributes) -> Markup
cardify' (h, hatts) t (b, batts) =
  Markup "div" ([("class","card")] <> hatts)
  [ MarkupLeaf $
    Markup "div" ([("class", "card")] <> hatts)
    (h <>
     [ MarkupLeaf $
       Markup "div" ([("class","body")] <> batts)
       (maybe mempty
         (\c -> [MarkupLeaf $ Markup "h5"
                [("class", "card-title")]
                  [Content c]]) t <>
          b)])]

-- | wrap some html with a classed div
divClass_ :: Text -> Html () -> Html ()
divClass_ t = with div_ [class__ t]

-- | A Html object based on the bootstrap accordion card concept.
accordionCard :: Bool -> [Attribute] -> Text -> Text -> Text -> Text -> Html () -> Html ()
accordionCard collapse atts idp idh idb t0 b =
  with div_ ([class__ "card"] <> atts) $
    with
      div_
      [class__ "card-header p-0", id_ idh]
      ( with
          h2_
          [class__ "m-0"]
          (with button_ [class__ ("btn btn-link" <> bool "" " collapsed" collapse), type_ "button", data_ "toggle" "collapse", data_ "target" ("#" <> idb), makeAttribute "aria-expanded" (bool "true" "false" collapse), makeAttribute "aria-controls" idb] (toHtml t0))
      )
      <> with
        div_
        [id_ idb, class__ ("collapse" <> bool " show" "" collapse), makeAttribute "aria-labelledby" idh, data_ "parent" ("#" <> idp)]
        (with div_ [class__ "card-body"] b)

-- | A bootstrap accordion card attached to a checkbox.
accordionCardChecked :: Bool -> Text -> Text -> Text -> Text -> Html () -> Html () -> Html ()
accordionCardChecked collapse idp idh idb label bodyhtml checkhtml =
  with div_ [class__ "card"] $
    with
      div_
      [class__ "card-header p-0", id_ idh]
      ( checkhtml
          <> with
            h2_
            [class__ "m-0"]
            (with button_ [class__ ("btn btn-link" <> bool "" " collapsed" collapse), type_ "button", data_ "toggle" "collapse", data_ "target" ("#" <> idb), makeAttribute "aria-expanded" (bool "true" "false" collapse), makeAttribute "aria-controls" idb] (toHtml label))
      )
      <> with
        div_
        [id_ idb, class__ ("collapse" <> bool " show" "" collapse), makeAttribute "aria-labelledby" idh, data_ "parent" ("#" <> idp)]
        (with div_ [class__ "card-body"] bodyhtml)

-- | create a bootstrapped accordian class
accordion ::
  (MonadState Int m) =>
  Text ->
  -- | name prefix.  This is needed because an Int doesn't seem to be a valid name.
  Maybe Text ->
  -- | card title
  [(Text, Html ())] ->
  -- | title, html tuple for each item in the accordion.
  m (Html ())
accordion pre x hs = do
  idp' <- genNamePre pre
  with div_ [class__ "accordion m-1", id_ idp'] <$> aCards idp'
  where
    aCards par = mconcat <$> mapM (aCard par) hs
    aCard par (t, b) = do
      idh <- genNamePre pre
      idb <- genNamePre pre
      pure $ accordionCard (x /= Just t) [] par idh idb t b

-- | create a bootstrapped accordian class
accordionChecked :: (MonadState Int m) => Text -> [(Text, Html (), Html ())] -> m (Html ())
accordionChecked pre hs = do
  idp' <- genNamePre pre
  with div_ [class__ "accordion m-1", id_ idp'] <$> aCards idp'
  where
    aCards par = mconcat <$> mapM (aCard par) hs
    aCard par (l, bodyhtml, checkhtml) = do
      idh <- genNamePre pre
      idb <- genNamePre pre
      pure $ accordionCardChecked True par idh idb l bodyhtml checkhtml

-- | This version of accordion runs a local state for naming, and will cause name clashes if the prefix is not unique.
accordion_ :: Text -> Maybe Text -> [(Text, Html ())] -> Html ()
accordion_ pre x hs = runIdentity $ evalStateT (accordion pre x hs) 0
