<h1 align="center">
    <a href="https://github.com/tonyday567/lucid-page">
        Lucid-Page
    </a>
</h1>

<hr>

Representation of a web page, based on lucid.

## Develop

The libray requires a bespoke version of the mvc library which can be found in the [mvc - dev branch][]. It also requires [mvc-extended][], [time-extended][] and [pipes-extended][].


``` sh
$ git clone https://github.com/tonyday567/mvc-extended.git
$ git clone https://github.com/tonyday567/time-extended.git
$ git clone https://github.com/tonyday567/pipes-extended.git
$ git clone https://github.com/tonyday567/Haskell-MVC-Library.git
$ cd Haskell-MVC-Library && git checkout dev && cd ..
$ git clone https://github.com/tonyday567/lucid-page.git
$ cd lucid-page
$ cabal sandbox init
$ cabal sandbox add-source ../mvc-extended
$ cabal sandbox add-source ../time-extended
$ cabal sandbox add-source ../pipes-extended
$ cabal sandbox add-source ../Haskell-MVC-Library
$ cabal configure --enable-tests
$ cabal install --dependencies-only --dry-run
$ cabal install --dependencies-only
$ cabal build
$ dist/build/test/test
```

[mvc - dev branch]: https://github.com/tonyday567/Haskell-MVC-Library/tree/dev
[mvc-extended]: https://github.com/tonyday567/mvc-extended
[time-extended]: https://github.com/tonyday567/time-extended
[pipes-extended]: https://github.com/tonyday567/pipes-extended
