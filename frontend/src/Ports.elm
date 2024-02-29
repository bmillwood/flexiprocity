port module Ports exposing (..)

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

facebookApi : { path : String, params : List (String, Json.Encode.Value), internal : Json.Encode.Value } -> Cmd msg
facebookApi { path, params, internal } =
  Json.Encode.object
    [ ("kind", Json.Encode.string "facebook-api")
    , ("path", Json.Encode.string path)
    , ("params", Json.Encode.object params)
    , ("internal", internal)
    ]
  |> sendToJS

moreFacebookFriends : { userId : String, paging : Maybe { after : String, prev : List Json.Decode.Value } } -> Cmd msg
moreFacebookFriends { userId, paging } =
  facebookApi
    { path = "/" ++ userId ++ "/friends"
    , -- If I add more fields here, remember to check the privacy policy
      params =
        [("fields", Json.Encode.string "id,name,short_name,picture,link")]
        ++ case paging of
          Just { after } -> [("after", Json.Encode.string after)]
          Nothing -> []
    , internal =
        Json.Encode.object
          [ ("id", Json.Encode.string "friends")
          , ("userId", Json.Encode.string userId)
          , ( "prevPages"
            , Json.Encode.list identity
              <| case paging of
                Just { prev } -> prev
                Nothing -> []
            )
          ]
    }

facebookFriends : { userId : String } -> Cmd msg
facebookFriends { userId } = moreFacebookFriends { userId = userId, paging = Nothing }

facebookUser : { personId : String } -> Cmd msg
facebookUser { personId } =
  -- for testing
  if String.startsWith "_" personId
  then Cmd.none
  else
    facebookApi
      { path = "/" ++ personId
      , -- If I add more fields here, remember to check the privacy policy
        params = [("fields", Json.Encode.string "id,name,short_name,picture,link")]
      , internal = Json.Encode.object [("id", Json.Encode.string "user")]
      }

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
  | FacebookFriends (List FacebookUser)
  | InProgress (Cmd ())

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
          Json.Decode.at ["request", "internal", "id"] Json.Decode.string
          |> Json.Decode.andThen (\id ->
            Json.Decode.oneOf
              [ case id of
                  "friends" ->
                    let
                      decodePagesWith decoder =
                        Json.Decode.at ["request", "internal", "prevPages"] (Json.Decode.list decoder)
                      decodeFriendsWith decoder =
                        Json.Decode.at ["response", "data"] (Json.Decode.list decoder)
                      decodePaging =
                        Json.Decode.map2
                          (\hasNext after -> Maybe.map (\() -> after) hasNext)
                          (Json.Decode.maybe (Json.Decode.field "next" (Json.Decode.succeed ())))
                          (Json.Decode.at ["cursors", "after"] Json.Decode.string)
                      resolve userId prevPages thisPage paging =
                        case paging of
                          Nothing ->
                            Json.Decode.map2
                              (\prevFriends friends -> Ok (FacebookFriends (friends ++ prevFriends)))
                              (decodePagesWith decodeUser)
                              (decodeFriendsWith decodeUser)
                          Just after ->
                            moreFacebookFriends
                              { userId = userId
                              , paging = Just { after = after, prev = thisPage ++ prevPages }
                              }
                            |> Json.Decode.succeed << Ok << InProgress
                    in
                    Json.Decode.map4 resolve
                      (Json.Decode.at ["request", "internal", "userId"] Json.Decode.string)
                      (decodePagesWith Json.Decode.value)
                      (decodeFriendsWith Json.Decode.value)
                      (Json.Decode.at ["response", "paging"] decodePaging)
                    |> Json.Decode.andThen identity
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
