{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# OPTIONS_GHC -Wall #-}

module Web.Page.Bootstrap where

import Web.Page.Types
import Web.Page.Html
import Lucid
import Lucid.Base
import Protolude

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

cardify :: [(Text, Text)] -> Html () -> Maybe Text -> Html () -> Html ()
cardify atts h t b =
  with div_ ([class_ "card"] <> toAtts atts) $
   h <>
   with
   div_ [class_ "card-body"]
   (maybe mempty (with h5_ [class_ "card-title"] . toHtml) t <>
    b)

container :: Html () -> Html ()
container = with div_ [class_ "container"]

row :: Html () -> Html ()
row = with div_ [class_ "row"]

col :: Html () -> Html ()
col = with div_ [class_ "col"]

colSm :: Html () -> Html ()
colSm = with div_ [class_ "col-sm"]

accordianCard :: Bool -> [(Text, Text)] -> Text -> Text -> Text -> Text -> Html () -> Html ()
accordianCard collapse atts idp idh idb t0 b =
  with div_ ([class_ "card"] <> toAtts atts) $
    (with div_ [class_ "card-header", id_ idh]
      (with h2_ [class_ "mb-0"]
        (with button_ [class_ ("btn btn-link" <> bool "" " collapsed" collapse), type_ "button", data_ "toggle" "collapse", data_ "target" ("#" <> idb), makeAttribute "aria-expanded" (bool "true" "false" collapse), makeAttribute "aria-controls" idb ] (toHtml t0)))) <>
    with div_ [id_ idb, class_ ("collapse" <> bool " show" "" collapse), makeAttribute "aria-labelledby" idh, data_ "parent" ("#" <> idp)]
    (with div_ [class_ "card-body"] b)

genName :: (MonadState Int m, Monad m) => m Text
genName = do
  modify (+1)
  show <$> get

accordion :: (MonadState Int m, Monad m) => Text -> Maybe Text -> [(Text, Html ())] -> m (Html ())
accordion idp x hs = do
  idp' <- (idp <>) <$> genName
  with div_ [class_ "accordion", id_ idp'] <$> aCards idp'
    where
      aCards par = mconcat <$> sequence (aCard par <$> hs)
      aCard par (t,b) = do
        idh <- (idp <>) <$> genName
        idb <- (idp <>) <$> genName
        pure $ accordianCard (maybe True (/=t) x) [] par idh idb t b


