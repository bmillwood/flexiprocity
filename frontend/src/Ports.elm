port module Ports exposing (..)

import Dict
import Json.Decode
import Json.Encode

port sendToJS : Json.Encode.Value -> Cmd msg
port receiveFromJS : (Json.Decode.Value -> msg) -> Sub msg

facebookLogin : Cmd msg
facebookLogin =
  Json.Encode.object [ ("kind", Json.Encode.string "facebook-login") ]
  |> sendToJS

facebookFriends : { userId : String } -> Cmd msg
facebookFriends { userId } =
  Json.Encode.object
    [ ("kind", Json.Encode.string "facebook-friends")
    , ("userId", Json.Encode.string userId)
    ]
  |> sendToJS

type FromJS
  = DriverProtocolError String
  | FacebookLoginUpdate { userId : Maybe String }
  | FacebookFriends (List Json.Decode.Value)

fromJS : Json.Decode.Decoder FromJS
fromJS =
  let
    decodePayload kind =
      case kind of
        "facebook-login-status" ->
          Json.Decode.field "status" Json.Decode.string
          |> Json.Decode.andThen (\status ->
            if status == "connected"
            then
              Json.Decode.at ["authResponse", "userID"] Json.Decode.string
              |> Json.Decode.map (\id -> FacebookLoginUpdate { userId = Just id })
            else Json.Decode.succeed (FacebookLoginUpdate { userId = Nothing })
          )
        "facebook-friends" ->
          Json.Decode.field "data"
            (Json.Decode.list Json.Decode.value)
          |> Json.Decode.map FacebookFriends
        _ -> Json.Decode.fail ("Unknown kind: " ++ kind)
  in
  Json.Decode.field "kind" Json.Decode.string
  |> Json.Decode.andThen (\kind ->
    Json.Decode.field "payload" (decodePayload kind)
  )

subscriptions : Sub FromJS
subscriptions =
  receiveFromJS <| \value ->
    case Json.Decode.decodeValue fromJS value of
      Ok msg -> msg
      Err error -> DriverProtocolError (Json.Decode.errorToString error)
