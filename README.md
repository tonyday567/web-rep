[web-page](https://tonyday567.github.io/web-page/index.html) [![Build Status](https://travis-ci.org/tonyday567/web-page.svg)](https://travis-ci.org/tonyday567/web-page)
===

Various functions and data type for a web page.

A web page is a product type consisting of:

  - html
  - css
  - js

recipes
---

```
stack build --test --exec "$(stack path --local-install-root)/bin/page-example" --file-watch
stack build web-page:test:test --file-watch --ghc-options -freverse-errors
```

reference
---

- beam me up [scotty](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/flags.html#flag-reference)
- [scotty-starter](https://github.com/scotty-web/scotty-starter)
- get [bootstrap](https://getbootstrap.com/)
- [bootstrap-slider](https://seiyria.com/bootstrap-slider)
- [blaze](http://hackage.haskell.org/package/blaze-html)
- [lucid](http://hackage.haskell.org/package/lucid)
- [clay](https://www.stackage.org/clay)
- [javascript-bridge](https://github.com/ku-fpg/javascript-bridge)



