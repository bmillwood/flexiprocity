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
  | FacebookConnected { userId : String, accessToken: String }
  | FacebookLoginFailed
  | FacebookFriends (List Json.Decode.Value)

fromJS : Json.Decode.Decoder FromJS
fromJS =
  let
    decodeAuthResponse =
      Json.Decode.map2 (\i t -> { userId = i, accessToken = t })
        (Json.Decode.field "userID" Json.Decode.string)
        (Json.Decode.field "accessToken" Json.Decode.string)
    decodePayload kind =
      case kind of
        "facebook-login-status" ->
          Json.Decode.field "status" Json.Decode.string
          |> Json.Decode.andThen (\status ->
            if status == "connected"
            then
              Json.Decode.field "authResponse" decodeAuthResponse
              |> Json.Decode.map FacebookConnected
            else Json.Decode.succeed FacebookLoginFailed
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
