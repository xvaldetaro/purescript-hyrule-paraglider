let conf = ../spago.dhall

in conf // {
  sources = conf.sources # [ "pages/**/*.purs" ],
  dependencies = conf.dependencies # [ "deku" ]
}
