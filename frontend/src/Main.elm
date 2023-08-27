module Main exposing (main)

import Browser

import Model
import View

main =
  Browser.document
    { init = Model.init
    , view = View.view
    , update = Model.update
    , subscriptions = Model.subscriptions
    }
