{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wall #-}

module Web.Page.Bootstrap
  ( bootstrapPage
  , cardify
  , b_
  , accordion
  ) where

import Lucid hiding (b_)
import Lucid.Base
import Protolude
import Web.Page.Html
import Web.Page.Types

bootstrapCss :: [Html ()]
bootstrapCss =
  [link_
  [ rel_ "stylesheet"
  , href_ "https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/css/bootstrap.min.css"
  , integrity_ "sha384-ggOyR0iXCbMQv3Xipma34MD+dH/1fQ784/j6cY/iJTQUOhcWr7x9JvoRxT2MZw1T"
  , crossorigin_ "anonymous"
  ]]

bootstrapJs :: [Html ()]
bootstrapJs =
  [ with (script_ mempty)
    [ src_ "https://code.jquery.com/jquery-3.3.1.slim.min.js"
    , integrity_ "sha384-q8i/X+965DzO0rT7abK41JStQIAqVgRVzpbzo5smXKp4YfRvH+8abtTE1Pi6jizo"
    , crossorigin_ "anonymous"
    ]
  , with (script_ mempty)
    [ src_ "https://cdnjs.cloudflare.com/ajax/libs/popper.js/1.14.7/umd/popper.min.js"
    , integrity_ "sha384-UO2eT0CpHqdSJQ6hJty5KVphtPhzWj9WO1clHTMGa3JDZwrnQq4sF86dIHNDz0W1"
    , crossorigin_ "anonymous"
    ]
  , with (script_ mempty)
    [ src_ "https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/js/bootstrap.min.js"
    , integrity_ "sha384-JjSmVgyd0p3pXB1rRibZUAYoIIy6OrQ6VrjIEaFf/nJGzIxFDsf4x0xIM+B07jRM"
    , crossorigin_ "anonymous"
    ]
  ]

bootstrapMeta :: [Html ()]
bootstrapMeta =
  [ meta_ [charset_ "utf-8"]
  , meta_ [ name_ "viewport"
          , content_ "width=device-width, initial-scale=1, shrink-to-fit=no"]
  ]

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

cardify :: [Attribute] -> Html () -> Maybe Text -> Html () -> Html ()
cardify atts h t b =
  with div_ ([class__ "card"] <> atts) $
   h <>
   with
   div_ [class__ "card-body"]
   (maybe mempty (with h5_ [class__ "card-title"] . toHtml) t <>
    b)

b_ :: Text -> Html () -> Html ()
b_ t = with div_ [class__ t]

accordionCard :: Bool -> [Attribute] -> Text -> Text -> Text -> Text -> Html () -> Html ()
accordionCard collapse atts idp idh idb t0 b =
  with div_ ([class__ "card"] <> atts) $
    with div_ [class__ "card-header", id_ idh]
      (with h2_ [class__ "mb-0"]
        (with button_ [class__ ("btn btn-link" <> bool "" " collapsed" collapse), type_ "button", data_ "toggle" "collapse", data_ "target" ("#" <> idb), makeAttribute "aria-expanded" (bool "true" "false" collapse), makeAttribute "aria-controls" idb ] (toHtml t0))) <>
    with div_ [id_ idb, class__ ("collapse" <> bool " show" "" collapse), makeAttribute "aria-labelledby" idh, data_ "parent" ("#" <> idp)]
    (with div_ [class__ "card-body"] b)

accordion :: (MonadState Int m, Monad m) => Text -> Maybe Text -> [(Text, Html ())] -> m (Html ())
accordion pre x hs = do
  idp' <- genNamePre pre
  with div_ [class__ "accordion", id_ idp'] <$> aCards idp'
    where
      aCards par = mconcat <$> sequence (aCard par <$> hs)
      aCard par (t,b) = do
        idh <- genNamePre pre
        idb <- genNamePre pre
        pure $ accordionCard (maybe True (/=t) x) [] par idh idb t b
