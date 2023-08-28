module Main exposing (main)

import Browser

import Model
import View

main =
  Browser.application
    { init = Model.init
    , view = View.view
    , update = Model.update
    , subscriptions = Model.subscriptions
    , onUrlRequest = Model.onUrlRequest
    , onUrlChange = Model.onUrlChange
    }
