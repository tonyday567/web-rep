
# Table of Contents

1.  [web-rep](#orgebbc06e)
2.  [library reference](#org8d29302)
3.  [Development](#orgddf3f50)


<a id="orgebbc06e"></a>

# web-rep

[![img](https://img.shields.io/hackage/v/web-rep.svg)](https://hackage.haskell.org/package/web-rep) [![img](https://github.com/tonyday567/web-rep/actions/workflows/haskell-ci.yml/badge.svg?branch=main)](https://github.com/tonyday567/web-rep/actions)

Various functions and representations for a web page.

The best way to understand functionality is via running the example app:

    cabal install
    page-example --apptype SharedTest

&#x2026; and then tune in to:

<http://localhost:9160/>


<a id="org8d29302"></a>

# library reference

-   [scotty](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/flags.html#flag-reference)
-   [bootstrap](https://getbootstrap.com/)
-   [bootstrap-slider](https://seiyria.com/bootstrap-slider)


<a id="orgddf3f50"></a>

# Development

    (setq haskell-process-args-cabal-repl '("web-rep:exe:web-rep-example"))

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />
</colgroup>
<tbody>
<tr>
<td class="org-left">web-rep:exe:web-rep-example</td>
</tr>
</tbody>
</table>

    :r
    :set -Wno-type-defaults
    :set -Wno-name-shadowing
    :set -XOverloadedStrings
    :set -XOverloadedLabels
    :set -XDataKinds
    import Prelude
    import Box
    import Web.Rep
    import Optics.Core
    import FlatParse.Basic
    import MarkupParse
    putStrLn "ok"

    Ok, 11 modules loaded.
    ghci
    ok

