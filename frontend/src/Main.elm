module Main exposing (..)

import Browser
import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events
import Http
import Json.Decode
import Json.Encode

import Ports

type LoginStatus a
  = Unknown
  | NotLoggedIn { pending : Bool }
  | LoggedIn a

type alias Model =
  { errors : List String
  , facebookLoggedIn : LoginStatus { userId : String, accessToken : String }
  , apiLoggedIn : LoginStatus { userId : String }
  , facebookFriends : Maybe (List Json.Decode.Value)
  }

type Msg
  = AddError String
  | FromJS Ports.FromJS
  | StartFacebookLogin
  | CheckApiLogin
  | ApiLoginResult (LoginStatus { userId : String })

init : () -> (Model, Cmd Msg)
init () =
  ( { errors = []
    , facebookLoggedIn = Unknown
    , apiLoggedIn = Unknown
    , facebookFriends = Nothing
    }
  , checkApiLogin
  )

view : Model -> Browser.Document Msg
view { errors, facebookLoggedIn, apiLoggedIn, facebookFriends } =
  let
    viewError err = Html.li [] [Html.text err]
  in
  { title = "Flexiprocity"
  , body =
      [ Html.ul [] (List.map viewError errors)
      , Html.p [] [
          case facebookLoggedIn of
            Unknown -> Html.text "Facebook init not completed"
            NotLoggedIn { pending } ->
              Html.button
                [ Events.onClick StartFacebookLogin
                , Attributes.disabled pending
                ]
                [ Html.text "Log in to Facebook" ]
            LoggedIn _ ->
              Html.text "Logged in to Facebook"
        ]
      , Html.p [] [
          case apiLoggedIn of
            Unknown -> Html.text "Checking API login..."
            NotLoggedIn _ -> Html.text "API not logged in"
            LoggedIn { userId } ->
              Html.text ("API user ID: " ++ userId)
        ]
      , Html.p [] [
          case facebookFriends of
            Nothing -> Html.text "Don't know friends yet"
            Just friends ->
              [ "Read "
              , String.fromInt (List.length friends)
              , " friends from Facebook"
              ] |> String.concat |> Html.text
        ]
      ]
  }

handleHttpResult : Result Http.Error Msg -> Msg
handleHttpResult r =
  case r of
    Err e -> AddError (Debug.toString e)
    Ok msg -> msg

checkApiLogin : Cmd Msg
checkApiLogin =
  let
    query = "mutation L{getOrCreateUserId(input: {}) {userId}}"
    body =
      [ ("query", Json.Encode.string query)
      , ("operationName", Json.Encode.string "L")
      , ("variables", Json.Encode.object [])
      ] |> Json.Encode.object
    decodeResult =
      Json.Decode.at
        ["data", "getOrCreateUserId", "userId"]
        (Json.Decode.nullable Json.Decode.string)
      |> Json.Decode.map (\u ->
        case u of
          Nothing -> ApiLoginResult (NotLoggedIn { pending = False })
          Just userId -> ApiLoginResult (LoggedIn { userId = userId })
      )
  in
  Http.post
    { url = "/graphql"
    , body = Http.jsonBody body
    , expect = Http.expectJson handleHttpResult decodeResult
    }

apiLogin : { accessToken : String } -> Cmd Msg
apiLogin { accessToken } =
  let
    decodeResult = Json.Decode.succeed CheckApiLogin
  in
  Http.post
    { url = "/login/facebook"
    , body =
        [ ("userToken", Json.Encode.string accessToken) ]
        |> Json.Encode.object |> Http.jsonBody
    , expect = Http.expectJson handleHttpResult decodeResult
    }

tryApiLogin : Model -> (Model, Cmd Msg)
tryApiLogin model =
  let
    needToSend =
      case model.apiLoggedIn of
        LoggedIn _ -> False
        Unknown -> False -- checkApiLogin probably inflight
        NotLoggedIn { pending } -> not pending
  in
  if needToSend
  then case model.facebookLoggedIn of
    LoggedIn { accessToken } ->
      ( { model | apiLoggedIn = NotLoggedIn { pending = True } }
      , apiLogin { accessToken = accessToken }
      )
    _ -> (model, Cmd.none)
  else (model, Cmd.none)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    AddError err ->
      ({ model | errors = err :: model.errors }, Cmd.none)
    FromJS (Ports.DriverProtocolError err) ->
      ({ model | errors = err :: model.errors }, Cmd.none)
    FromJS (Ports.FacebookConnected params) ->
      let
        (newModel, cmd) = tryApiLogin { model | facebookLoggedIn = LoggedIn params }
      in
      ( newModel
      , [ case newModel.facebookFriends of
            Nothing -> Ports.facebookFriends { userId = params.userId }
            Just _ -> Cmd.none
        , cmd
        ] |> Cmd.batch
      )
    FromJS Ports.FacebookLoginFailed ->
      ({ model | facebookLoggedIn = NotLoggedIn { pending = False } }, Cmd.none)
    FromJS (Ports.FacebookFriends friends) ->
      ({ model | facebookFriends = Just friends }, Cmd.none)
    StartFacebookLogin ->
      let
        alreadyPending =
          case model.facebookLoggedIn of
            NotLoggedIn { pending } -> pending
            _ -> False
      in
      if alreadyPending
      then (model, Cmd.none)
      else
        ( { model | facebookLoggedIn = NotLoggedIn { pending = True } }
        , Ports.facebookLogin
        )
    CheckApiLogin ->
      (model, checkApiLogin)
    ApiLoginResult newState ->
      tryApiLogin { model | apiLoggedIn = newState }

subscriptions : Model -> Sub Msg
subscriptions model = Sub.map FromJS Ports.subscriptions

main =
  Browser.document
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }
