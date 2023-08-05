module Main exposing (..)

import Browser
import Html exposing (Html)
import Html.Events as Events
import Http
import Json.Decode
import Json.Encode

import Ports

type LoginStatus a
  = Unknown
  | NotLoggedIn
  | LoggedIn a

type alias Model =
  { errors : List String
  , facebookLoggedIn : LoginStatus { userId : String, accessToken : String }
  , ourLoggedIn : LoginStatus ()
  , facebookFriends : Maybe (List Json.Decode.Value)
  }

type Msg
  = AddError String
  | FromJS Ports.FromJS
  | StartFacebookLogin
  | OurLoginResult (LoginStatus ())

init : () -> (Model, Cmd Msg)
init () =
  ( { errors = []
    , facebookLoggedIn = Unknown
    , ourLoggedIn = Unknown
    , facebookFriends = Nothing
    }
  , Cmd.none
  )

view : Model -> Browser.Document Msg
view { errors, facebookLoggedIn, facebookFriends } =
  let
    viewError err = Html.li [] [Html.text err]
  in
  { title = "Flexiprocity"
  , body =
      [ Html.ul [] (List.map viewError errors)
      , Html.p [] [
          case facebookLoggedIn of
            Unknown -> Html.text "Facebook init not completed"
            NotLoggedIn ->
              Html.button
                [ Events.onClick StartFacebookLogin ]
                [ Html.text "Log in to Facebook" ]
            LoggedIn _ ->
              Html.text "Logged in to Facebook"
        ]
      , case facebookFriends of
          Nothing -> Html.text "Don't know friends yet"
          Just friends ->
            [ "Read "
            , String.fromInt (List.length friends)
            , " friends from Facebook"
            ] |> String.concat |> Html.text
      ]
  }

getFriendsIfNecessary : Model -> { userId : String } -> Cmd msg
getFriendsIfNecessary model { userId } =
  case model.facebookFriends of
    Nothing -> Ports.facebookFriends { userId = userId }
    Just _ -> Cmd.none

ourLoginIfNecessary : Model -> { accessToken : String } -> Cmd Msg
ourLoginIfNecessary model { accessToken } =
  let
    decodeResult = Json.Decode.succeed (LoggedIn ())
    handleResult r =
      case r of
        Err err -> AddError (Debug.toString err)
        Ok result -> OurLoginResult result
  in
  case model.ourLoggedIn of
    LoggedIn _ -> Cmd.none
    _ ->
      Http.post
        { url = "/login/facebook"
        , body =
            [ ("userToken", Json.Encode.string accessToken) ]
            |> Json.Encode.object |> Http.jsonBody
        , expect = Http.expectJson handleResult decodeResult
        }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    AddError err ->
      ({ model | errors = err :: model.errors }, Cmd.none)
    FromJS (Ports.DriverProtocolError err) ->
      ({ model | errors = err :: model.errors }, Cmd.none)
    FromJS (Ports.FacebookConnected params) ->
      ( { model | facebookLoggedIn = LoggedIn params }
      , [ getFriendsIfNecessary model { userId = params.userId }
        , ourLoginIfNecessary model { accessToken = params.accessToken }
        ] |> Cmd.batch
      )
    FromJS Ports.FacebookLoginFailed ->
      ({ model | facebookLoggedIn = NotLoggedIn }, Cmd.none)
    FromJS (Ports.FacebookFriends friends) ->
      ({ model | facebookFriends = Just friends }, Cmd.none)
    StartFacebookLogin ->
      (model, Ports.facebookLogin)
    OurLoginResult newState ->
      ({ model | ourLoggedIn = newState }, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model = Sub.map FromJS Ports.subscriptions

main =
  Browser.document
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }
