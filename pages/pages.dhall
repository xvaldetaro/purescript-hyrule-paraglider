let conf = ../spago.dhall

in conf // {
  sources = conf.sources # [ "pages/**/*.purs" ],
  dependencies = conf.dependencies #
    [ "bolson"
    , "console"
    , "control"
    , "datetime"
    , "deku"
    , "monoid-extras"
    , "now"
    , "profunctor"
    , "record"
    , "routing-duplex"
    , "routing"
    , "strings"
    , "web-events"
    , "web-html"
    , "web-uievents"
    ]
}
