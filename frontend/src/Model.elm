module Model exposing (..)

import Dict exposing (Dict)
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

type alias FacebookId = String
type alias UserId = String
type alias WouldId = String

type alias Profile =
  { userId : UserId
  , facebookId : String
  , bio : String
  , audience : Audience
  , matchedWoulds : List { wouldId : WouldId }
  , youWould : List { wouldId : WouldId }
  }

decodeProfile : Json.Decode.Decoder Profile
decodeProfile =
  let
    decodeWouldIds =
      Json.Decode.list
        (Json.Decode.map (\i -> { wouldId = i }) Json.Decode.string)
  in
  Json.Decode.map6
    Profile
    (Json.Decode.field "userId" Json.Decode.string)
    (Json.Decode.field "facebookId" Json.Decode.string)
    (Json.Decode.field "bio" Json.Decode.string)
    (Json.Decode.field "audience" decodeAudience)
    (Json.Decode.field "matchedWoulds" decodeWouldIds)
    (Json.Decode.field "youWould" decodeWouldIds)

type alias Model =
  { errors : List String
  , facebookLoggedIn : LoginStatus { userId : FacebookId, accessToken : String }
  , apiLoggedIn : LoginStatus { userId : UserId }
  , facebookUsers : Dict FacebookId Ports.FacebookUser
  , profiles : Dict UserId Profile
  , facebookFriends : Maybe (List Ports.FacebookUser)
  , wouldsById : Dict WouldId String
  , myBio : String
  , myVisibility : Maybe Audience
  , showMe : Audience
  , wouldChange : Dict UserId (Dict WouldId Bool)
  }

type OneMsg
  = AddError String
  | FromJS Ports.FromJS
  | StartFacebookLogin
  | StartLogout
  | CheckApiLogin
  | ApiLoginResult (LoginStatus { userId : UserId })
  | Woulds (Dict WouldId String)
  | GotProfile Profile
  | EditBio String
  | SubmitBio
  | MyVisibility Audience
  | SubmitVisibility
  | ShowMe Audience
  | WouldChange { userId : UserId, wouldId : WouldId, changeTo : Bool }
  | SubmitWouldChanges

type alias Msg = List OneMsg

init : () -> (Model, Cmd Msg)
init () =
  ( { errors = []
    , facebookLoggedIn = Unknown
    , apiLoggedIn = Unknown
    , facebookUsers = Dict.empty
    , profiles = Dict.empty
    , facebookFriends = Nothing
    , wouldsById = Dict.empty
    , myBio = ""
    , myVisibility = Nothing
    , showMe = Friends
    , wouldChange = Dict.empty
    }
  , checkApiLogin
  )

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

profileFragment : String
profileFragment =
  "fragment F on UserProfile{userId facebookId bio audience matchedWoulds youWould}"

getProfiles
  :  { getMyVisibility : Bool, getWoulds : Bool, userId: UserId }
  -> Model -> Cmd Msg
getProfiles { getMyVisibility, getWoulds, userId } model =
  let
    decodeProfiles =
      Json.Decode.list decodeProfile
      |> Json.Decode.map (List.map GotProfile)
    decodeVisible =
      Json.Decode.at ["data", "myUser", "visibleTo"] decodeAudience
      |> Json.Decode.map (List.singleton << MyVisibility)
    decodeWould =
      Json.Decode.map2
        (\i n -> (i, n))
        (Json.Decode.field "wouldId" Json.Decode.string)
        (Json.Decode.field "name" Json.Decode.string)
    decodeWoulds =
      Json.Decode.at ["data", "woulds", "nodes"]
        (Json.Decode.list decodeWould)
      |> Json.Decode.map (\woulds -> [Woulds (Dict.fromList woulds)])
  in
  graphQL
    { query =
        [ profileFragment
        , "query Q($u:BigInt!){"
          , "me:userProfiles(condition:{userId:$u}){nodes{...F}}"
          , let
              condition = case model.showMe of
                Self ->
                  -- doesn't really make sense, but we don't use this anyway
                  "(condition:{userId:$u})"
                Friends -> "(condition:{audience:FRIENDS})"
                Everyone -> ""
            in
            "them:userProfiles" ++ condition ++ "{nodes{...F}}"
          , if getMyVisibility then "myUser{visibleTo}" else ""
          , if getWoulds then "woulds{nodes{wouldId name}}" else ""
        , "}"
        ] |> String.concat
    , operationName = "Q"
    , variables =
        [ ("u", Json.Encode.string userId) ]
    , decodeResult =
        Json.Decode.map4
          (\me p v w -> List.concat [me, p, v, w])
          (Json.Decode.at ["data", "me", "nodes"] decodeProfiles)
          (Json.Decode.at ["data", "them", "nodes"] decodeProfiles)
          (if getMyVisibility then decodeVisible else Json.Decode.succeed [])
          (if getWoulds then decodeWoulds else Json.Decode.succeed [])
    }

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
      let
        addUser user acc = Dict.insert user.id user acc
        newUsers = List.foldr addUser model.facebookUsers friends
        newModel = { model | facebookFriends = Just friends, facebookUsers = newUsers }
      in
      (newModel, sendFriends newModel)
    FromJS (Ports.FacebookGotUser user) ->
      ( { model | facebookUsers = Dict.insert user.id user model.facebookUsers }
      , Cmd.none
      )
    FromJS (Ports.FacebookGotError (Ports.AccessTokenExpired { whileDoing })) ->
      let
        ignore =
          case model.facebookLoggedIn of
            LoggingOut -> True
            NotLoggedIn -> True
            LoggingIn -> False
            Unknown -> False
            LoggedIn _ -> False
      in
      if ignore
      then (model, Cmd.none)
      else
        ( { model
          | errors = ("Facebook auth failed for " ++ whileDoing) :: model.errors
          }
        , Cmd.none
        )
    FromJS (Ports.FacebookGotError (Ports.UnknownError { whileDoing, error })) ->
      ( { model
        | errors =
            ("Error (" ++ whileDoing ++ "): " ++ Json.Encode.encode 0 error)
            :: model.errors
        }
      , Cmd.none
      )
    StartFacebookLogin ->
      case model.facebookLoggedIn of
        LoggingIn -> (model, Cmd.none)
        _ ->
          ( { model | facebookLoggedIn = LoggingIn }
          , Ports.facebookLogin
          )
    StartLogout ->
      let
        newModel =
          { model
          | facebookLoggedIn = LoggingOut
          , facebookUsers = Dict.empty
          , profiles = Dict.empty
          , facebookFriends = Nothing
          , myBio = ""
          , myVisibility = Nothing
          , wouldChange = Dict.empty
          }
        apiLogout =
          case model.apiLoggedIn of
            LoggingOut -> Cmd.none
            _ ->
              let
                loggedOut () = [ ApiLoginResult NotLoggedIn ]
              in
              Http.request
                { method = "DELETE"
                , headers = []
                , url = "/login/facebook"
                , body = Http.emptyBody
                , expect =
                    Http.expectWhatever
                      (handleHttpResult << Result.map loggedOut)
                , timeout = Nothing
                , tracker = Nothing
                }
        facebookLogout =
          case model.facebookLoggedIn of
            LoggingOut -> Cmd.none
            _ -> Ports.facebookLogout
      in
      (newModel, Cmd.batch [apiLogout, facebookLogout])
    CheckApiLogin ->
      (model, checkApiLogin)
    ApiLoginResult newState ->
      let
        (newModel, cmd) = tryApiLogin { model | apiLoggedIn = newState }
        initialQuery userId =
          getProfiles
            { getMyVisibility = True, getWoulds = True, userId = userId }
            model
      in
      case newState of
        LoggedIn { userId } ->
          ( newModel
          , Cmd.batch [cmd, initialQuery userId, sendFriends newModel]
          )
        _ -> (newModel, cmd)
    Woulds woulds -> ({ model | wouldsById = woulds }, Cmd.none)
    GotProfile user ->
      let
        isMe otherId =
          case model.apiLoggedIn of
            LoggedIn { userId } -> userId == otherId
            _ -> False
        redundantChange =
          Maybe.andThen (\woulds ->
            let
              newWoulds =
                Dict.filter
                  (\wId changeTo ->
                    List.member { wouldId = wId } user.youWould /= changeTo
                  )
                  woulds
            in
            if Dict.isEmpty newWoulds
            then Nothing
            else Just newWoulds
          )
      in
      ( { model
        | profiles = Dict.insert user.userId user model.profiles
        , wouldChange = Dict.update user.userId redundantChange model.wouldChange
        , myBio = if isMe user.userId then user.bio else model.myBio
        }
      , Cmd.none
      )
    EditBio bio -> ({ model | myBio = bio }, Cmd.none)
    SubmitBio ->
      ( model
      , case model.apiLoggedIn of
          LoggedIn { userId } ->
            graphQL
              { query = "mutation B($b:String!,$u:BigInt!){updateMe(input:{bio:$b}){query{userProfiles(condition:{userId:$u}){nodes{userId facebookId bio audience matchedWoulds youWould}}}}}"
              , operationName = "B"
              , variables =
                  [ ("b", Json.Encode.string model.myBio)
                  , ("u", Json.Encode.string userId)
                  ]
              , decodeResult =
                  Json.Decode.at ["data", "updateMe", "query", "userProfiles", "nodes"]
                    (Json.Decode.list decodeProfile)
                  |> Json.Decode.map (List.map GotProfile)
              }
          _ -> Cmd.none
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
    ShowMe who ->
      let
        newModel = { model | showMe = who }
      in
      ( newModel
      , case model.apiLoggedIn of
          LoggedIn { userId } ->
            getProfiles
              { getMyVisibility = False, getWoulds = False, userId = userId }
              newModel
          _ -> Cmd.none
      )
    WouldChange { userId, wouldId, changeTo } ->
      let
        isAlready =
          case Dict.get userId model.profiles of
            Nothing -> False
            Just p -> List.member { wouldId = wouldId } p.youWould == changeTo
        doChange =
          if isAlready
          then Dict.remove wouldId
          else Dict.insert wouldId changeTo
        justIfNonEmpty d = if Dict.isEmpty d then Nothing else Just d
        change v =
          Maybe.withDefault Dict.empty v
          |> doChange
          |> justIfNonEmpty
      in
      ( { model | wouldChange = Dict.update userId change model.wouldChange }
      , Cmd.none
      )
    SubmitWouldChanges ->
      case model.apiLoggedIn of
        LoggedIn { userId } ->
          ( model
          , Dict.toList model.wouldChange
            |> List.map (\(uid, woulds) ->
                Dict.toList woulds
                |> List.map (\(wId, changeTo) ->
                    graphQL
                      { query =
                          [ profileFragment
                          , "mutation C($u:BigInt!,$wo:BigInt!,$wi:BigInt!){"
                          , if changeTo
                            then "createUserWould(input:{userWould:"
                            else "deleteUserWould(input:"
                          , "{userId:$u,wouldId:$wo,withId:$wi}"
                          , if changeTo
                            then "})"
                            else ")"
                          , "{query{userProfiles(condition:{userId:$wi}){"
                          , "nodes{...F}"
                          , "}}}"
                          , "}"
                          ] |> String.concat
                      , operationName = "C"
                      , variables =
                          [ ("u", Json.Encode.string userId)
                          , ("wo", Json.Encode.string wId)
                          , ("wi", Json.Encode.string uid)
                          ]
                      , decodeResult =
                          Json.Decode.at
                            [ "data"
                            , if changeTo then "createUserWould" else "deleteUserWould"
                            , "query"
                            , "userProfiles"
                            , "nodes"
                            ]
                            (Json.Decode.list decodeProfile)
                          |> Json.Decode.map (List.map GotProfile)
                      }
                  )
              )
            |> List.concat
            |> Cmd.batch
          )
        _ -> (model, Cmd.none)

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
