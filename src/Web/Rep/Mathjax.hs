{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wall #-}

-- | mathjax functionality.
--
-- some <https://docs.mathjax.org/en/latest/web/start.html mathjax> assets
--
-- <https://math.meta.stackexchange.com/questions/5020/mathjax-basic-tutorial-and-quick-reference mathjax cheatsheet>
--
-- FIXME: Mathjax inside svg doesn't quite work, and needs to be structured around the foreign object construct.
module Web.Rep.Mathjax
  ( mathjaxPage,
    mathjax27Page,
    mathjaxSvgPage,
    mathjax27SvgPage,
  )
where

import Control.Lens
import Lucid
import NumHask.Prelude
import Text.InterpolatedString.Perl6
import Web.Rep.Page

mathjax3Lib :: Html ()
mathjax3Lib =
  with
    (script_ mempty)
    [ src_ "https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-svg.js",
      id_ "MathJax-script",
      async_ ""
    ]

mathjax27Lib :: Html ()
mathjax27Lib =
  with
    (script_ mempty)
    [ src_ "https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.0/MathJax.js?config=TeX-MML-AM_SVG"
    ]

jqueryLib :: Html ()
jqueryLib =
  with
    (script_ mempty)
    [ src_ "https://code.jquery.com/jquery-3.3.1.slim.min.js",
      integrity_ "sha384-q8i/X+965DzO0rT7abK41JStQIAqVgRVzpbzo5smXKp4YfRvH+8abtTE1Pi6jizo",
      crossorigin_ "anonymous"
    ]

-- | A 'Page' that loads mathjax
mathjaxPage :: Page
mathjaxPage =
  mempty
    & #jsGlobal .~ RepJsText scriptMathjaxConfig
    & #libsJs
      .~ [ mathjax3Lib
         ]

-- | A 'Page' that loads the mathjax 2.7 js library
mathjax27Page :: Page
mathjax27Page =
  mempty
    & #jsOnLoad .~ RepJsText scriptMathjaxConfig
    & #libsJs
      .~ [ mathjax27Lib
         ]

-- | A 'Page' that tries to enable mathjax inside svg (which is tricky).
mathjaxSvgPage :: Text -> Page
mathjaxSvgPage cl =
  mempty
    & #jsGlobal .~ RepJsText (scriptMathjaxConfigSvg cl)
    & #libsJs
      .~ [ mathjax3Lib,
           jqueryLib
         ]

-- | A 'Page' that tries to enable mathjax 2.7 inside svg.
mathjax27SvgPage :: Text -> Page
mathjax27SvgPage cl =
  mempty
    & #jsGlobal .~ RepJsText (scriptMathjax27ConfigSvg cl)
    & #libsJs
      .~ [ mathjax27Lib,
           jqueryLib
         ]

scriptMathjaxConfig :: Text
scriptMathjaxConfig =
  [q|
MathJax = {
  tex: {
    inlineMath: [ ['$','$'], ["\\(","\\)"] ]
  }
};
|]

-- | Mathjax applies within an svg element as normal, but results in an svg element inside a text element which is not allowed, hence the extra scripting.
-- http://bl.ocks.org/larsenmtl/86077bddc91c3de8d3db6a53216b2f47
scriptMathjax27ConfigSvg :: Text -> Text
scriptMathjax27ConfigSvg cl =
  [qq|
     setTimeout(() => \{

         MathJax.Hub.Config(\{
             tex2jax: \{
                 inlineMath: [ ['$','$'], ["\\(","\\)"] ],
                 processEscapes: true
             }
         });

         MathJax.Hub.Register.StartupHook("End", function() \{
             setTimeout(() => \{
                 $('.{cl}').each(function()\{
                     var m = $('text>span>svg');
                     m.remove();
                     $(this).append(m);
                 });

             }, 1);
         });
     }, 1);
|]

-- | Mathjax applies within an svg element as normal, but results in an svg element inside a text element which is not allowed, hence the extra scripting.
-- http://bl.ocks.org/larsenmtl/86077bddc91c3de8d3db6a53216b2f47
scriptMathjaxConfigSvg :: Text -> Text
scriptMathjaxConfigSvg cl =
  [qq|
        window.MathJax = \{
             tex: \{
                 inlineMath: [ ['$','$'], ["\\(","\\)"] ]
             },
             startup: \{
                 ready: () => \{
                     MathJax.startup.defaultReady();
                     MathJax.startup.promise.then(() => \{
                         var xs = document.querySelectorAll('.{cl}');
                         xs.forEach((x) =>
                             \{ x.querySelectorAll('text').forEach((t) =>
                                 \{t.querySelectorAll('mjx-container>svg').forEach((s) =>
                                     \{
                                         Array.from(t.attributes).forEach((a) => s.setAttribute(a.name, a.value));
                                         x.appendChild(s);
                                 });
                             });
                         });
                     });
                 }
             }
         };
|]
