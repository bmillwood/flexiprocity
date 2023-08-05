module Main exposing (..)

import Browser
import Html exposing (Html)
import Html.Events as Events
import Json.Decode

import Ports

type FacebookLogin
  = Unknown
  | NotLoggedIn
  | LoggedIn { userId : String }

type alias Model =
  { errors : List String
  , facebookLoggedIn : FacebookLogin
  , facebookFriends : Maybe (List Json.Decode.Value)
  }

type Msg
  = FromJS Ports.FromJS
  | StartFacebookLogin

init : () -> (Model, Cmd Msg)
init () =
  ( { errors = []
    , facebookLoggedIn = Unknown
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

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    FromJS (Ports.DriverProtocolError err) ->
      ({ model | errors = err :: model.errors }, Cmd.none)
    FromJS (Ports.FacebookLoginUpdate { userId }) ->
      let
        (newLogin, cmd) = case userId of
          Nothing -> (NotLoggedIn, Cmd.none)
          Just uid ->
            ( LoggedIn { userId = uid }
            , case model.facebookFriends of
                Nothing -> Ports.facebookFriends { userId = uid }
                Just _ -> Cmd.none
            )
      in
      ({ model | facebookLoggedIn = newLogin }, cmd)
    FromJS (Ports.FacebookFriends friends) ->
      ({ model | facebookFriends = Just friends }, Cmd.none)
    StartFacebookLogin ->
      (model, Ports.facebookLogin)

subscriptions : Model -> Sub Msg
subscriptions model = Sub.map FromJS Ports.subscriptions

main =
  Browser.document
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }
