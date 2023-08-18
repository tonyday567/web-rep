{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- | Some <https://getbootstrap.com/ bootstrap> assets and functionality.
module Web.Rep.Bootstrap
  ( bootstrapCss,
    bootstrapJs,
    bootstrapMeta,
    bootstrapPage,
    cardify,
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
import MarkupParse
import Web.Rep.Page
import Web.Rep.Shared
import Data.ByteString (ByteString)
import Data.Tree

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Web.Rep
-- >>> import Data.Markup

bootstrapCss :: [Tree Token]
bootstrapCss = pure $ pure $
  tag "link"
      [ Attr "rel" "stylesheet",
        Attr "href" "https://cdn.jsdelivr.net/npm/bootstrap@5.0.2/dist/css/bootstrap.min.css",
        Attr "integrity" "sha384-EVSTQN3/azprG1Anm3QDgpJLIm9Nao0Yz1ztcQTwFspd3yD65VohhpuuCOmLASjC",
        Attr "crossorigin" "anonymous"
      ]

bootstrapJs :: [Tree Token]
bootstrapJs = pure <$>
  [ StartTag "script"
      [ Attr "src" "https://cdn.jsdelivr.net/npm/bootstrap@5.0.2/dist/js/bootstrap.bundle.min.js",
        Attr "integrity" "sha384-MrcW6ZMFYlzcLA8Nl+NtUVF0sA7MsXsP1UyJoMp4YLEuNSfAP+JcXn/tWtIaxVXM",
        Attr "crossorigin" "anonymous"
      ],
    StartTag "script"
      [ Attr "src" "https://code.jquery.com/jquery-3.3.1.slim.min.js",
        Attr "integrity" "sha384-q8i/X+965DzO0rT7abK41JStQIAqVgRVzpbzo5smXKp4YfRvH+8abtTE1Pi6jizo",
        Attr "crossorigin" "anonymous"
      ]
  ]

bootstrapMeta :: [Tree Token]
bootstrapMeta = pure <$>
  [ tag "meta" [Attr "charset" "utf-8"],
    tag "meta"
      [ Attr "name" "viewport",
        Attr "content" "width=device-width, initial-scale=1, shrink-to-fit=no"
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
    bootstrapMeta
    mempty

-- | wrap some Html with the bootstrap <https://getbootstrap.com/docs/4.3/components/card/ card> class
cardify :: ([Tree Token], [Attr]) -> Maybe ByteString -> ([Tree Token], [Attr]) -> [Tree Token]
cardify (h, hatts) t (b, batts) =
  pure $ wrap "div" ([Attr "class" "card"] <> hatts) $
    h <> [ wrap "div" ([Attr "class" "card-body"] <> batts)
        ( maybe mempty (pure . wrap "h5" [Attr "class" "card-title"] . pure . pure . Content) t <> b
        ) ]

-- | wrap some html with a classed div
div :: ByteString -> [Tree Token] -> Tree Token
div c xs = wrap "div" [Attr "class" c] xs

-- | A Html object based on the bootstrap accordion card concept.
accordionCard :: Bool -> [Attr] -> ByteString -> ByteString -> ByteString -> ByteString -> [Tree Token] -> Tree Token
accordionCard collapse atts idp idh idb t0 b =
  wrap "div" ([Attr "class" "card"] <> atts) $
      pure (wrap "div" [Attr "class" "card-header p-0", Attr "id" idh]
        [ wrap "h2" [Attr "class" "m-0"]
          (pure $ wrap "button"
           [ Attr "class" ("btn btn-link" <> bool "" " collapsed" collapse),
             Attr "type" "button",
             Attr "data-toggle" "collapse",
             Attr "data-target" ("#" <> idb),
             Attr "aria-expanded" (bool "true" "false" collapse),
             Attr "aria-controls" idb
           ] (pure $ pure $ Content t0)),
          wrap "div"
        [Attr "id" "idb",
         Attr "class" ("collapse" <> bool " show" "" collapse),
         Attr "aria-labelledby" idh,
         Attr "data-parent" ("#" <> idp)
        ] (pure $ wrap "div" [Attr "class" "card-body"] b)
        ])

-- | A bootstrap accordion card attached to a checkbox.
accordionCardChecked :: Bool -> ByteString -> ByteString -> ByteString -> ByteString -> [Tree Token] -> [Tree Token] -> Tree Token
accordionCardChecked collapse idp idh idb label bodyhtml checkhtml =
  wrap "div" [Attr "class" "card"] $
    [ wrap "div"
      [Attr "class" "card-header p-0",
       Attr "id" idh
      ]
      ( checkhtml
          <> ( pure $ wrap "h2"
            [Attr "class" "m-0"]
            (pure $ wrap "button"
             [ Attr "class" ("btn btn-link" <> bool "" " collapsed" collapse),
               Attr "type" "button",
               Attr "data-toggle" "collapse",
               Attr "data-target" ("#" <> idb),
               Attr "aria-expanded" (bool "true" "false" collapse),
               Attr "aria-controls" idb
             ] (pure $ pure $ Content label))
 )      ),
       wrap "div"
        [Attr "id" "idb",
         Attr "class" ("collapse" <> bool " show" "" collapse),
         Attr "aria-labelledby" idh,
         Attr "data-parent" ("#" <> idp)
        ]
        (pure $ wrap "div" [Attr "class" "card-body"] bodyhtml)
    ]

-- | create a bootstrapped accordian class
accordion ::
  (MonadState Int m) =>
  ByteString ->
  -- | name prefix.  This is needed because an Int doesn't seem to be a valid name.
  Maybe ByteString ->
  -- | card title
  [(ByteString, [Tree Token])] ->
  -- | title, html tuple for each item in the accordion.
  m (Tree Token)
accordion pre x hs = do
  idp' <- genNamePre pre
  wrap "div" [Attr "class" "accordion m-1", Attr "id" idp'] <$> (aCards idp')
  where
    aCards par = mapM (aCard par) hs
    aCard par (t, b) = do
      idh <- genNamePre pre
      idb <- genNamePre pre
      pure $ accordionCard (x /= Just t) [] par idh idb t b

-- | create a bootstrapped accordian class
accordionChecked :: (MonadState Int m) => ByteString -> [(ByteString, [Tree Token], [Tree Token])] -> m (Tree Token)
accordionChecked pre hs = do
  idp' <- genNamePre pre
  wrap "div" [Attr "class" "accordion m-1", Attr "id" idp'] <$> (aCards idp')
  where
    aCards par = mapM (aCard par) hs
    aCard par (l, bodyhtml, checkhtml) = do
      idh <- genNamePre pre
      idb <- genNamePre pre
      pure $ accordionCardChecked True par idh idb l bodyhtml checkhtml

-- | This version of accordion runs a local state for naming, and will cause name clashes if the prefix is not unique.
accordion_ :: ByteString -> Maybe ByteString -> [(ByteString, [Tree Token])] -> Tree Token
accordion_ pre x hs = runIdentity $ evalStateT (accordion pre x hs) 0
