let conf = ../spago.dhall

in conf // {
  sources = conf.sources # [ "pages/**/*.purs" ],
  dependencies = conf.dependencies #
    [ "bolson"
    , "console"
    , "control"
    , "deku"
    , "monoid-extras"
    , "record"
    , "routing-duplex"
    , "routing"
    , "strings"
    , "web-events"
    , "web-html"
    , "web-uievents"
    ]
}
