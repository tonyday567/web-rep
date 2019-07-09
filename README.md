[web-rep](https://github.com/tonyday567/web-rep) [![Build Status](https://travis-ci.org/tonyday567/web-rep.svg)](https://travis-ci.org/tonyday567/web-rep)
===

Various functions and representations for a web page.


recipes
---

```
stack build --test --exec "$(stack path --local-install-root)/bin/page-example --midtype Prod" --file-watch
stack build web-rep:test:test --file-watch --ghc-options -freverse-errors
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
