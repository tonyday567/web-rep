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
import Lucid
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
