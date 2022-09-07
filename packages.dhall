let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.2-20220621/packages.dhall
        sha256:78caab14e4d8ff3886a057f0380c2d4a2500e2ee7ab5c1d32a0f9ce5c71eedd8

let overrides =
  { hyrule =
    { dependencies =
      [ "arrays"
      , "control"
      , "datetime"
      , "effect"
      , "either"
      , "filterable"
      , "foldable-traversable"
      , "js-timers"
      , "lists"
      , "maybe"
      , "monoid-extras"
      , "newtype"
      , "now"
      , "ordered-collections"
      , "partial"
      , "prelude"
      , "profunctor"
      , "record"
      , "refs"
      , "st"
      , "tuples"
      , "unsafe-coerce"
      , "unsafe-reference"
      , "web-events"
      , "web-html"
      , "web-uievents"
      ]
    , repo = "https://github.com/mikesol/purescript-hyrule.git"
    , version = "v2.2.0"
    }
  , monoid-extras =
    { dependencies = [ "profunctor-lenses" ]
    , repo = "https://github.com/mikesol/purescript-monoid-extras.git"
    , version = "v0.0.1"
    }
  }

in  upstream // overrides
