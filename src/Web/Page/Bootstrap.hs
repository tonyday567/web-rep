{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

-- | Some <https://getbootstrap.com/ bootstrap> assets and functionality.
module Web.Page.Bootstrap
  ( bootstrapPage,
    cardify,
    divClass_,
    accordion,
    accordionChecked,
    accordionCard,
    accordionCardChecked,
    accordion_,
  )
where

import Control.Monad.State
import Data.Bool
import Data.Functor.Identity
import Data.Text (Text)
import Lucid
import Lucid.Base
import Web.Page.Html
import Web.Page.Types
import Prelude

bootstrapCss :: [Html ()]
bootstrapCss =
  [ link_
      [ rel_ "stylesheet",
        href_ "https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/css/bootstrap.min.css",
        integrity_ "sha384-ggOyR0iXCbMQv3Xipma34MD+dH/1fQ784/j6cY/iJTQUOhcWr7x9JvoRxT2MZw1T",
        crossorigin_ "anonymous"
      ]
  ]

bootstrapJs :: [Html ()]
bootstrapJs =
  [ with
      (script_ mempty)
      [ src_ "https://code.jquery.com/jquery-3.3.1.slim.min.js",
        integrity_ "sha384-q8i/X+965DzO0rT7abK41JStQIAqVgRVzpbzo5smXKp4YfRvH+8abtTE1Pi6jizo",
        crossorigin_ "anonymous"
      ],
    with
      (script_ mempty)
      [ src_ "https://cdnjs.cloudflare.com/ajax/libs/popper.js/1.14.7/umd/popper.min.js",
        integrity_ "sha384-UO2eT0CpHqdSJQ6hJty5KVphtPhzWj9WO1clHTMGa3JDZwrnQq4sF86dIHNDz0W1",
        crossorigin_ "anonymous"
      ],
    with
      (script_ mempty)
      [ src_ "https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/js/bootstrap.min.js",
        integrity_ "sha384-JjSmVgyd0p3pXB1rRibZUAYoIIy6OrQ6VrjIEaFf/nJGzIxFDsf4x0xIM+B07jRM",
        crossorigin_ "anonymous"
      ]
  ]

bootstrapMeta :: [Html ()]
bootstrapMeta =
  [ meta_ [charset_ "utf-8"],
    meta_
      [ name_ "viewport",
        content_ "width=device-width, initial-scale=1, shrink-to-fit=no"
      ]
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
  with div_ ([class__ "card"]) $
    with
      div_
      ([class__ "card-header p-0", id_ idh])
      ( checkhtml
          <> with
            h2_
            [class__ "m-0"]
            (with button_ [class__ ("btn btn-link" <> bool "" " collapsed" collapse), type_ "button", data_ "toggle" "collapse", data_ "target" ("#" <> idb), makeAttribute "aria-expanded" (bool "true" "false" collapse), makeAttribute "aria-controls" idb] (toHtml label))
      )
      <> with
        div_
        [id_ idb, class__ ("collapse" <> bool " show" "" collapse), makeAttribute "aria-labelledby" idh, data_ "parent" ("#" <> idp)]
        (with div_ ([class__ "card-body"]) bodyhtml)

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
    aCards par = mconcat <$> sequence (aCard par <$> hs)
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
    aCards par = mconcat <$> sequence (aCard par <$> hs)
    aCard par (l, bodyhtml, checkhtml) = do
      idh <- genNamePre pre
      idb <- genNamePre pre
      pure $ accordionCardChecked True par idh idb l bodyhtml checkhtml

-- | This version of accordion runs a local state for naming, and will cause name clashes if the prefix is not unique.
accordion_ :: Text -> Maybe Text -> [(Text, Html ())] -> Html ()
accordion_ pre x hs = runIdentity $ evalStateT (accordion pre x hs) 0
