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

facebookLogout : Cmd msg
facebookLogout =
  Json.Encode.object [ ("kind", Json.Encode.string "facebook-logout") ]
  |> sendToJS

facebookApi : { path : String, id : String } -> Cmd msg
facebookApi { path, id } =
  Json.Encode.object
    [ ("kind", Json.Encode.string "facebook-api")
    , ("path", Json.Encode.string path)
    , ("id", Json.Encode.string id)
    ]
  |> sendToJS

facebookFriends : { userId : String } -> Cmd msg
facebookFriends { userId } =
  facebookApi { path = "/" ++ userId ++ "/friends", id = "friends" }

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
          ) |> Json.Decode.field "response"
        "facebook-api" ->
          Json.Decode.field "id" Json.Decode.string
          |> Json.Decode.andThen (\id ->
            case id of
              "friends" ->
                Json.Decode.at ["response", "data"]
                  (Json.Decode.list Json.Decode.value)
                |> Json.Decode.map FacebookFriends
              _ -> Json.Decode.fail ("Unknown api req id: " ++ id)
          )
        _ -> Json.Decode.fail ("Unknown kind: " ++ kind)
  in
  Json.Decode.field "kind" Json.Decode.string
  |> Json.Decode.andThen decodePayload

subscriptions : Sub FromJS
subscriptions =
  receiveFromJS <| \value ->
    case Json.Decode.decodeValue fromJS value of
      Ok msg -> msg
      Err error -> DriverProtocolError (Json.Decode.errorToString error)
