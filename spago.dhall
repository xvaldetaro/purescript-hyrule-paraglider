{-
You can edit this file as you like.

Need help? See the following resources:
- Spago documentation: https://github.com/purescript/spago
- Dhall language tour: https://docs.dhall-lang.org/tutorials/Language-Tour.html

When creating a new Spago project, you can use
`spago init --no-comments` or `spago init -C`
to generate this file without the comments in this block.
-}
{ name = "hyrule-paraglider"
, dependencies =
  [ "aff"
  , "arrays"
  , "bifunctors"
  , "control"
  , "datetime"
  , "effect"
  , "either"
  , "exceptions"
  , "filterable"
  , "foldable-traversable"
  , "heterogeneous"
  , "hyrule"
  , "integers"
  , "js-timers"
  , "lists"
  , "maybe"
  , "now"
  , "ordered-collections"
  , "prelude"
  , "record"
  , "refs"
  , "st"
  , "tuples"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
, license = "MIT"
, repository = "https://github.com/xvaldetaro/purescript-hyrule-paraglider"
}
