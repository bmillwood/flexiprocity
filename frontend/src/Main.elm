module Main exposing (..)

import Browser
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events
import Http
import Json.Decode
import Json.Encode

import Ports

type LoginStatus a
  = Unknown
  | NotLoggedIn
  | LoggingIn
  | LoggedIn a
  | LoggingOut

type alias ApiUser =
  { id : String
  , facebookId : Maybe String
  , name : Maybe String
  , bio : String
  }

decodeApiUser : Json.Decode.Decoder ApiUser
decodeApiUser =
  Json.Decode.map4
    (\i f n b -> { id = i, facebookId = f, name = n, bio = b })
    (Json.Decode.field "userId" Json.Decode.string)
    (Json.Decode.field "facebookId" (Json.Decode.nullable Json.Decode.string))
    (Json.Decode.field "name" (Json.Decode.nullable Json.Decode.string))
    (Json.Decode.field "bio" Json.Decode.string)

type alias Model =
  { errors : List String
  , facebookLoggedIn : LoginStatus { userId : String, accessToken : String }
  , apiLoggedIn : LoginStatus { userId : String }
  , facebookUsers : Dict String Ports.FacebookUser
  , apiUsers : Dict String ApiUser
  , facebookFriends : Maybe (List Json.Decode.Value)
  , myBio : String
  }

type Msg
  = AddError String
  | FromJS Ports.FromJS
  | StartFacebookLogin
  | StartFacebookLogout
  | CheckApiLogin
  | ApiLoginResult (LoginStatus { userId : String })
  | ApiUsers (List ApiUser)
  | EditBio String
  | SubmitBio

init : () -> (Model, Cmd Msg)
init () =
  ( { errors = []
    , facebookLoggedIn = Unknown
    , apiLoggedIn = Unknown
    , facebookUsers = Dict.empty
    , apiUsers = Dict.empty
    , facebookFriends = Nothing
    , myBio = ""
    }
  , checkApiLogin
  )

view : Model -> Browser.Document Msg
view model =
  let
    viewError err = Html.li [] [Html.text err]
    viewUser user isMe =
      let
        facebookUser =
          user.facebookId
          |> Maybe.andThen (\fbid -> Dict.get fbid model.facebookUsers)
        name =
          case facebookUser of
            Nothing -> Maybe.withDefault "[unknown]" user.name
            Just u -> u.name
        picture = facebookUser |> Maybe.map .picture
      in
      Html.div
        [ Attributes.style "display" "flex"
        , Attributes.class "user"
        ]
        [ case picture of
            Nothing -> Html.div [ Attributes.height 50, Attributes.width 50 ] []
            Just p ->
              Html.img
                [ Attributes.src p.url
                , Attributes.height p.height
                , Attributes.width p.width
                ]
                []
        , Html.div
            []
            [ Html.div [] [Html.text name]
            , Html.div [Attributes.class "user-bio"] (
                if isMe
                then
                  [ Html.input
                      [ Attributes.type_ "text"
                      , Attributes.placeholder "short bio"
                      , Attributes.value model.myBio
                      , Events.onInput EditBio
                      ]
                      []
                  , let
                      saved = user.bio == model.myBio
                    in
                    Html.button
                      [ Events.onClick SubmitBio
                      , Attributes.disabled saved
                      ]
                      [ Html.text (if saved then "Saved" else "Save") ]
                  ]
                else
                  [ Html.text user.bio ]
              )
            ]
        ]
  in
  { title = "Flexiprocity"
  , body =
      [ [ Html.h1 [] [ Html.text "flexiprocity" ]
        , Html.ul [] (List.map viewError model.errors)
        , let
            button disabled text =
              Html.button
                [ Events.onClick StartFacebookLogin
                , Attributes.disabled disabled
                , Attributes.class "facebook-button"
                , Attributes.class "facebook-login"
                ]
                [ Html.text text ]
            viewLoggedIn userId =
              case Dict.get userId model.facebookUsers of
                Nothing -> [ Html.text "Logged in with Facebook" ]
                Just user ->
                  [ Html.text ("Logged in as " ++ user.shortName ++ " ") ]
          in
          case model.facebookLoggedIn of
            Unknown -> Html.p [] [button True "Checking Facebook login status"]
            NotLoggedIn -> Html.p [] [ button False "Login with Facebook" ]
            LoggingIn -> Html.p [] [ button True "Logging in..." ]
            LoggedIn { userId } ->
              Html.p []
                [ Html.span
                    [ Attributes.class "facebook-button"
                    , Attributes.class "facebook-logged-in"
                    ]
                    (viewLoggedIn userId)
                , Html.button
                    [ Attributes.class "facebook-button"
                    , Attributes.class "facebook-logout"
                    , Events.onClick StartFacebookLogout
                    ]
                    [ Html.text "Logout" ]
                ]
            LoggingOut ->
              Html.p []
                [ Html.button
                    [ Attributes.class "facebook-button"
                    , Attributes.class "facebook-logout"
                    , Attributes.disabled True
                    ]
                    [ Html.text "Logging out..." ]
                ]
        ]
      , case model.apiLoggedIn of
          LoggedIn { userId } ->
            case Dict.get userId model.apiUsers of
              Just u -> [viewUser u True]
              Nothing -> []
          _ -> []
      , [ Html.p []
            [
              case model.facebookFriends of
                Nothing -> Html.text "Fetching Facebook friends..."
                Just friends ->
                  [ if List.isEmpty friends
                    then "None"
                    else String.fromInt (List.length friends)
                  , " of your Facebook friends use Flexiprocity"
                  , if List.isEmpty friends
                    then " 🙁"
                    else ""
                  ] |> String.concat |> Html.text
            ]
        ]
      ] |> List.concat
  }

handleHttpResult : Result Http.Error Msg -> Msg
handleHttpResult r =
  case r of
    Err e -> AddError (Debug.toString e)
    Ok msg -> msg

graphQL
  :  { query : String
     , operationName : String
     , variables : List (String, Json.Encode.Value)
     , decodeResult : Json.Decode.Decoder Msg
     }
  -> Cmd Msg
graphQL { query, operationName, variables, decodeResult } =
  let
    body =
      [ ("query", Json.Encode.string query)
      , ("operationName", Json.Encode.string operationName)
      , ("variables", Json.Encode.object variables)
      ] |> Json.Encode.object
  in
  Http.post
    { url = "/graphql"
    , body = Http.jsonBody body
    , expect = Http.expectJson handleHttpResult decodeResult
    }

checkApiLogin : Cmd Msg
checkApiLogin =
  let
    decodeResult =
      Json.Decode.at
        ["data", "getOrCreateUserId", "userId"]
        (Json.Decode.nullable Json.Decode.string)
      |> Json.Decode.map (\u ->
        case u of
          Nothing -> ApiLoginResult NotLoggedIn
          Just userId -> ApiLoginResult (LoggedIn { userId = userId })
      )
  in
  graphQL
    { query = "mutation L{getOrCreateUserId(input: {}) {userId}}"
    , operationName = "L"
    , variables = []
    , decodeResult = decodeResult
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
        Unknown -> False -- checkApiLogin probably inflight
        NotLoggedIn -> True
        LoggingIn -> False
        LoggedIn _ -> False
        LoggingOut -> True
  in
  if needToSend
  then case model.facebookLoggedIn of
    LoggedIn { accessToken } ->
      ( { model | apiLoggedIn = LoggingIn }
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
        , Ports.facebookUser { personId = params.userId }
        , cmd
        ] |> Cmd.batch
      )
    FromJS Ports.FacebookLoginFailed ->
      ({ model | facebookLoggedIn = NotLoggedIn }, Cmd.none)
    FromJS (Ports.FacebookFriends friends) ->
      ({ model | facebookFriends = Just friends }, Cmd.none)
    FromJS (Ports.FacebookGotUser user) ->
      ( { model | facebookUsers = Dict.insert user.id user model.facebookUsers }
      , Cmd.none
      )
    StartFacebookLogin ->
      case model.facebookLoggedIn of
        LoggingIn -> (model, Cmd.none)
        _ ->
          ( { model | facebookLoggedIn = LoggingIn }
          , Ports.facebookLogin
          )
    StartFacebookLogout ->
      case model.facebookLoggedIn of
        LoggingOut -> (model, Cmd.none)
        _ ->
          ( { model | facebookLoggedIn = LoggingOut }
          , Ports.facebookLogout
          )
    CheckApiLogin ->
      (model, checkApiLogin)
    ApiLoginResult newState ->
      let
        (newModel, cmd) = tryApiLogin { model | apiLoggedIn = newState }
        lookupMe userId =
          graphQL
            { query = "query Q($u:BigInt!){userProfiles(condition:{userId:$u}){nodes{userId facebookId name bio}}}"
            , operationName = "Q"
            , variables = [("u", Json.Encode.string userId)]
            , decodeResult =
                Json.Decode.at ["data", "userProfiles", "nodes"]
                  (Json.Decode.list decodeApiUser)
                |> Json.Decode.map ApiUsers
            }
      in
      case newState of
        LoggedIn { userId } ->
          (newModel, Cmd.batch [cmd, lookupMe userId])
        _ -> (newModel, cmd)
    ApiUsers users ->
      let
        isMe otherId =
          case model.apiLoggedIn of
            LoggedIn { userId } -> userId == otherId
            _ -> False
        addUser user accModel =
          { accModel
          | apiUsers = Dict.insert user.id user accModel.apiUsers
          , myBio = if isMe user.id then Debug.log "bio" user.bio else accModel.myBio
          }
      in
      ( List.foldl addUser model users
      , Cmd.none
      )
    EditBio bio -> ({ model | myBio = bio }, Cmd.none)
    SubmitBio ->
      ( model
      , graphQL
          { query = "mutation B($b:String!){updateMe(input:{bio:$b}){user{userId facebookId name bio}}}"
          , operationName = "B"
          , variables = [("b", Json.Encode.string model.myBio)]
          , decodeResult =
              Json.Decode.at ["data", "updateMe", "user"] decodeApiUser
              |> Json.Decode.map (ApiUsers << List.singleton)
          }
      )

subscriptions : Model -> Sub Msg
subscriptions model = Sub.map FromJS Ports.subscriptions

main =
  Browser.document
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }
