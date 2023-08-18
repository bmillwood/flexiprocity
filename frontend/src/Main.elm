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
  , facebookId : String
  , bio : String
  }

decodeApiUser : Json.Decode.Decoder ApiUser
decodeApiUser =
  Json.Decode.map3
    (\i f b -> { id = i, facebookId = f, bio = b })
    (Json.Decode.field "userId" Json.Decode.string)
    (Json.Decode.field "facebookId" Json.Decode.string)
    (Json.Decode.field "bio" Json.Decode.string)

type Audience
  = Self
  | Friends
  | Everyone

audienceToString : Audience -> String
audienceToString audience =
  case audience of
    Self -> "SELF"
    Friends -> "FRIENDS"
    Everyone -> "EVERYONE"

encodeAudience : Audience -> Json.Encode.Value
encodeAudience = Json.Encode.string << audienceToString

decodeAudience : Json.Decode.Decoder Audience
decodeAudience =
  Json.Decode.string
  |> Json.Decode.andThen (\audience ->
    case audience of
      "SELF" -> Json.Decode.succeed Self
      "FRIENDS" -> Json.Decode.succeed Friends
      "EVERYONE" -> Json.Decode.succeed Everyone
      _ -> Json.Decode.fail ("unknown audience: " ++ audience)
  )

type alias Model =
  { errors : List String
  , facebookLoggedIn : LoginStatus { userId : String, accessToken : String }
  , apiLoggedIn : LoginStatus { userId : String }
  , facebookUsers : Dict String Ports.FacebookUser
  , apiUsers : Dict String ApiUser
  , facebookFriends : Maybe (List Ports.FacebookUser)
  , wouldsByName : Dict String { id : String }
  , myBio : String
  , myVisibility : Maybe Audience
  }

type OneMsg
  = AddError String
  | FromJS Ports.FromJS
  | StartFacebookLogin
  | StartFacebookLogout
  | CheckApiLogin
  | ApiLoginResult (LoginStatus { userId : String })
  | Woulds (Dict String { id : String })
  | GotApiUser ApiUser
  | EditBio String
  | SubmitBio
  | MyVisibility Audience
  | SubmitVisibility

type alias Msg = List OneMsg

init : () -> (Model, Cmd Msg)
init () =
  ( { errors = []
    , facebookLoggedIn = Unknown
    , apiLoggedIn = Unknown
    , facebookUsers = Dict.empty
    , apiUsers = Dict.empty
    , facebookFriends = Nothing
    , wouldsByName = Dict.empty
    , myBio = ""
    , myVisibility = Nothing
    }
  , checkApiLogin
  )

view : Model -> Browser.Document Msg
view model =
  let
    viewError err = Html.li [] [Html.text err]
    viewUser user isMe =
      let
        facebookUser = Dict.get user.facebookId model.facebookUsers
        name =
          Maybe.map .name facebookUser
          |> Maybe.withDefault ("[fbid " ++ user.facebookId ++ "]")
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
                      , Events.onInput (List.singleton << EditBio)
                      ]
                      []
                  , let
                      saved = user.bio == model.myBio
                    in
                    Html.button
                      [ Events.onClick [SubmitBio]
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
  { title = "flexiprocity"
  , body =
      [ [ Html.h1 [] [ Html.text "flexiprocity" ]
        , Html.ul [] (List.map viewError model.errors)
        ]
        , let
            button disabled text =
              Html.button
                [ Events.onClick [StartFacebookLogin]
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
            Unknown ->
              [Html.p [] [button True "Checking Facebook login status"]]
            NotLoggedIn ->
              [Html.p [] [button False "Login with Facebook"]]
            LoggingIn ->
              [Html.p [] [button True "Logging in..."]]
            LoggedIn { userId } ->
              [ Html.p []
                  [ Html.span
                      [ Attributes.class "facebook-button"
                      , Attributes.class "facebook-logged-in"
                      ]
                      (viewLoggedIn userId)
                  , Html.button
                      [ Attributes.class "facebook-button"
                      , Attributes.class "facebook-logout"
                      , Events.onClick [StartFacebookLogout]
                      ]
                      [ Html.text "Logout" ]
                  ]
              , let
                  radio v s =
                    [ Html.input
                      [ Attributes.type_ "radio"
                      , Attributes.name "visibility"
                      , Attributes.id ("visible-" ++ audienceToString v)
                      , Attributes.checked (model.myVisibility == Just v)
                      , Events.onCheck (\_ -> [MyVisibility v, SubmitVisibility])
                      ] []
                    , Html.label
                        [ Attributes.for ("visible-" ++ audienceToString v) ]
                        [ Html.text s ]
                    ]
                in
                [ [ Html.text "Show my profile to people I've ticked and:" ]
                , radio Self "Nobody else"
                , radio Friends "Friends"
                , radio Everyone "Everyone"
                ] |> List.concat |> Html.p []
              ]
            LoggingOut ->
              [ Html.p []
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
                  , " of your Facebook friends use flexiprocity"
                  , if List.isEmpty friends
                    then " 🙁"
                    else ""
                  ] |> String.concat |> Html.text
            ]
        , Html.table
            [ Attributes.style "width" "100%"
            , Attributes.style "padding" "1em"
            ]
            [ Html.thead []
                [ let
                    wouldCols =
                      Dict.keys model.wouldsByName
                      |> List.map (\name ->
                        Html.th
                          [ Attributes.style "width" "10%" ]
                          [ Html.text name ]
                      )
                    cols =
                      Html.th
                        [ Attributes.style "text-align" "left" ]
                        [ Html.text "People" ]
                      :: wouldCols
                  in
                  Html.tr [] cols
                ]
            , Html.tbody [] []
            ]
        ]
      ] |> List.concat
  }

handleHttpResult : Result Http.Error Msg -> Msg
handleHttpResult r =
  case r of
    Err e -> [AddError (Debug.toString e)]
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
          Nothing -> [ApiLoginResult NotLoggedIn]
          Just userId -> [ApiLoginResult (LoggedIn { userId = userId })]
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
    decodeResult = Json.Decode.succeed [CheckApiLogin]
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

sendFriends : Model -> Cmd Msg
sendFriends model =
  case (model.myVisibility, model.apiLoggedIn, model.facebookFriends) of
    (Just Friends, LoggedIn _, Just friends) ->
      graphQL
        { query = "mutation F($f:[String]!){setFacebookFriends(input:{friendFbids:$f}){unit}}"
        , operationName = "F"
        , variables =
            [ ( "f"
              , Json.Encode.list (Json.Encode.string << .id) friends
              )
            ]
        , decodeResult = Json.Decode.succeed []
        }
    _ -> Cmd.none

updateOne : OneMsg -> Model -> (Model, Cmd Msg)
updateOne msg model =
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
      ({ model | facebookFriends = Just friends }, sendFriends model)
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
        decodeProfiles =
          Json.Decode.at ["data", "userProfiles", "nodes"]
            (Json.Decode.list decodeApiUser)
          |> Json.Decode.map (List.map GotApiUser)
        decodeVisible =
          Json.Decode.at ["data", "myUser", "visibleTo"] decodeAudience
          |> Json.Decode.map (List.singleton << MyVisibility)
        decodeWould =
          Json.Decode.map2
            (\i n -> (n, { id = i }))
            (Json.Decode.field "wouldId" Json.Decode.string)
            (Json.Decode.field "name" Json.Decode.string)
        decodeWoulds =
          Json.Decode.at ["data", "woulds", "nodes"]
            (Json.Decode.list decodeWould)
          |> Json.Decode.map (\woulds -> [Woulds (Dict.fromList woulds)])
        initialQuery userId =
          graphQL
            { query = "query Q($u:BigInt!){userProfiles(condition:{userId:$u}){nodes{userId facebookId bio}}myUser{visibleTo}woulds{nodes{wouldId name}}}"
            , operationName = "Q"
            , variables = [("u", Json.Encode.string userId)]
            , decodeResult =
                Json.Decode.map3
                  (\p v w -> List.concat [p, v, w])
                  decodeProfiles
                  decodeVisible
                  decodeWoulds
            }
      in
      case newState of
        LoggedIn { userId } ->
          ( newModel
          , Cmd.batch [cmd, initialQuery userId, sendFriends newModel]
          )
        _ -> (newModel, cmd)
    Woulds woulds -> ({ model | wouldsByName = woulds }, Cmd.none)
    GotApiUser user ->
      let
        isMe otherId =
          case model.apiLoggedIn of
            LoggedIn { userId } -> userId == otherId
            _ -> False
      in
      ( { model
        | apiUsers = Dict.insert user.id user model.apiUsers
        , myBio = if isMe user.id then user.bio else model.myBio
        }
      , Cmd.none
      )
    EditBio bio -> ({ model | myBio = bio }, Cmd.none)
    SubmitBio ->
      ( model
      , graphQL
          { query = "mutation B($b:String!){updateMe(input:{bio:$b}){user{userId facebookId bio}}}"
          , operationName = "B"
          , variables = [("b", Json.Encode.string model.myBio)]
          , decodeResult =
              Json.Decode.at ["data", "updateMe", "user"] decodeApiUser
              |> Json.Decode.map (List.singleton << GotApiUser)
          }
      )
    MyVisibility who ->
      let
        newModel = { model | myVisibility = Just who }
      in
      ( newModel
      , case model.myVisibility of
          Just Friends -> Cmd.none -- no need to resend
          _ -> sendFriends newModel
      )
    SubmitVisibility ->
      case model.myVisibility |> Maybe.map encodeAudience of
        Nothing -> (model, Cmd.none)
        Just v ->
          ( model
          , graphQL
              { query = "mutation V($a:Audience!){updateMe(input:{visibleTo:$a}){user{visibleTo}}}"
              , operationName = "V"
              , variables = [("a", v)]
              , decodeResult =
                  Json.Decode.at ["data", "updateMe", "user", "visibleTo"] decodeAudience
                  |> Json.Decode.map (List.singleton << MyVisibility)
              }
          )

update : Msg -> Model -> (Model, Cmd Msg)
update msgs model =
  case msgs of
    [] -> (model, Cmd.none)
    msg :: rest ->
      let
        (newModel, cmd) = updateOne msg model
        (finalModel, cmds) = update rest newModel
      in
      (finalModel, Cmd.batch [cmd, cmds])

subscriptions : Model -> Sub Msg
subscriptions model = Sub.map (List.singleton << FromJS) Ports.subscriptions

main =
  Browser.document
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }
