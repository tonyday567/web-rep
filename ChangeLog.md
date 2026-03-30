0.14.1
======

* Dependency maintenance: relaxed scotty < 1, transformers < 0.7
* Doctests: removed non-deterministic exception test due to GHC HasCallStack changes
* Updated tested-with to GHC 9.10.2, 9.12.3, 9.14.1

0.14
===

* Added Web.Rep.Internal.FlatParse (due to markup-parse-2.0 changes)

0.13
===
* added sharedPage
* modified to use modern bootstrap methods


0.12.1
===
* Changed the order of Page elements, so that inline css over-rides libraries.
* Added cssColorScheme to API

0.12
===
* markupInput replaces inputToHtml as per markup-parse
* ToByteString introduced
* upgrade to box-socket-0.5

0.11
===
* Removed clay, lucid as dependencies
* refactored to markup-parse as markup representation.

0.10.2
===

* GHC 9.6.2 upgrade

0.8.0
===

* Removed numhask dependencies
