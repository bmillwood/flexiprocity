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
  -- If I add more fields here, remember to check the privacy policy
  facebookApi
    { path = "/" ++ userId ++ "/friends?fields=id,name,short_name,picture,link"
    , id = "friends"
    }

facebookUser : { personId : String } -> Cmd msg
facebookUser { personId } =
  -- If I add more fields here, remember to check the privacy policy
  if String.startsWith "_" personId
  then Cmd.none
  else facebookApi { path = "/" ++ personId ++ "/?fields=id,name,short_name,picture,link", id = "user" }

sentry : { message : String } -> Cmd msg
sentry { message } =
  Json.Encode.object
    [ ("kind", Json.Encode.string "sentry")
    , ("message", Json.Encode.string message)
    ]
  |> sendToJS

type alias FacebookUser =
  { id : String
  , name : String
  , shortName : String
  , picture : { height : Int, width : Int, url : String }
  , link : Maybe String
  }

type Error
  = DriverProtocolError String
  | FacebookSDKLoadFailed
  | FacebookAccessTokenExpired { whileDoing : String }
  | FacebookUnknownError { whileDoing : String, error : Json.Decode.Value }

type Update
  = FacebookConnected { userId : String, accessToken: String }
  | FacebookLoginFailed
  | FacebookGotUser FacebookUser
  | FacebookFriends { data : List FacebookUser, hasPagination : Bool }

type alias FromJS = Result Error Update

fromJS : Json.Decode.Decoder FromJS
fromJS =
  let
    decodeAuthResponse =
      Json.Decode.map2 (\i t -> { userId = i, accessToken = t })
        (Json.Decode.field "userID" Json.Decode.string)
        (Json.Decode.field "accessToken" Json.Decode.string)
    decodePicture =
      Json.Decode.map3
        (\h w u -> { height = h, width = w, url = u })
        (Json.Decode.field "height" Json.Decode.int)
        (Json.Decode.field "width" Json.Decode.int)
        (Json.Decode.field "url" Json.Decode.string)
    decodeUser =
      Json.Decode.map5
         (\i n sn p l -> { id = i, name = n, shortName = sn, picture = p, link = l })
         (Json.Decode.field "id" Json.Decode.string)
         (Json.Decode.field "name" Json.Decode.string)
         (Json.Decode.field "short_name" Json.Decode.string)
         (Json.Decode.at ["picture", "data"] decodePicture)
         (Json.Decode.maybe (Json.Decode.field "link" Json.Decode.string))
    decodeApiError whileDoing =
      Json.Decode.field "code" Json.Decode.int
      |> Json.Decode.andThen (\code ->
        case code of
          190 ->
            FacebookAccessTokenExpired { whileDoing = whileDoing }
            |> Json.Decode.succeed
          _ ->
            Json.Decode.value
            |> Json.Decode.map (\v ->
              FacebookUnknownError { whileDoing = whileDoing, error = v }
            )
      )
      |> Json.Decode.map Err
    decodePayload kind =
      case kind of
        "facebook-sdk-load-failure" ->
          Json.Decode.succeed (Err FacebookSDKLoadFailed)
        "facebook-login-status" ->
          Json.Decode.field "status" Json.Decode.string
          |> Json.Decode.andThen (\status ->
            if status == "connected"
            then
              Json.Decode.field "authResponse" decodeAuthResponse
              |> Json.Decode.map (Ok << FacebookConnected)
            else Json.Decode.succeed (Ok FacebookLoginFailed)
          ) |> Json.Decode.field "response"
        "facebook-api" ->
          Json.Decode.field "id" Json.Decode.string
          |> Json.Decode.andThen (\id ->
            Json.Decode.oneOf
              [ case id of
                  "friends" ->
                    let
                      friends = Json.Decode.field "data" (Json.Decode.list decodeUser)
                      isJust x =
                        case x of
                          Just () -> True
                          Nothing -> False
                      paging =
                        Json.Decode.at ["paging", "next"] (Json.Decode.succeed ())
                        |> Json.Decode.maybe
                        |> Json.Decode.map isJust
                      ofFields d hp =
                        Ok (FacebookFriends { data = d, hasPagination = hp })
                      parseResponse =
                        Json.Decode.map2 ofFields friends paging
                    in
                    Json.Decode.field "response" parseResponse
                  "user" ->
                    Json.Decode.field "response" decodeUser
                    |> Json.Decode.map (Ok << FacebookGotUser)
                  _ -> Json.Decode.fail ("Unknown api req id: " ++ id)
              , Json.Decode.at ["response", "error"] (decodeApiError id)
              ]
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
      Err error -> Err (DriverProtocolError (Json.Decode.errorToString error))
