module Model exposing (..)

import Browser
import Browser.Navigation as Nav
import Dict exposing (Dict)
import Http
import Json.Decode
import Json.Encode
import Process
import Set exposing (Set)
import Task
import Url exposing (Url)
import Url.Parser exposing ((<?>))
import Url.Parser.Query

import ListZipper
import Ports
import SearchWords

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
  , facebookIds : List String
  , blueskyHandles : List String
  , hasGoogle : Bool
  , name : Maybe String
  , bio : String
  , picture : Maybe String
  , audience : Audience
  , friendsSince : Maybe String
  , createdAt : String
  , matchedWouldIds : Set WouldId
  , youWouldIds : Set WouldId
  }

decodeProfile : Json.Decode.Decoder Profile
decodeProfile =
  let
    decodeWouldIds =
      Json.Decode.list Json.Decode.string
      |> Json.Decode.map Set.fromList
  in
  Json.Decode.map5
    identity
    (Json.Decode.map8
      Profile
      (Json.Decode.field "userId" Json.Decode.string)
      (Json.Decode.field "facebookIds" (Json.Decode.list Json.Decode.string))
      (Json.Decode.field "blueskyHandles" (Json.Decode.list Json.Decode.string))
      (Json.Decode.field "hasGoogle" Json.Decode.bool)
      (Json.Decode.field "name" (Json.Decode.nullable Json.Decode.string))
      (Json.Decode.field "bio" Json.Decode.string)
      (Json.Decode.field "picture" (Json.Decode.nullable Json.Decode.string))
      (Json.Decode.field "audience" decodeAudience))
    (Json.Decode.field "friendsSince" (Json.Decode.nullable Json.Decode.string))
    (Json.Decode.field "createdAt" Json.Decode.string)
    (Json.Decode.field "matchedWoulds" decodeWouldIds)
    (Json.Decode.field "youWould" decodeWouldIds)

type Page
  = PageNotFound
  | Root
  | Columns
  | Account { deleteConfirmations : Set String }
  | Privacy
  | Unsubscribe
      { address : Maybe String
      , token : Maybe String
      , success : Bool
      , requestUnsubAddress : String
      }
  | Security
  | WhyNotFacebook
  | Test

accountPage : Page
accountPage = Account { deleteConfirmations = Set.empty }

type alias Would = { name : String, addedById : Maybe UserId, uses : Maybe Int }
type WouldUpdate
  = CreateWould { name : String }
  | RenameWould { id : WouldId, name : String }
  | DeleteWould { id : WouldId }

type Draggable = Column String
type alias DragTarget = Draggable

type alias Drag =
  { held : Draggable
  , over : Maybe DragTarget
  }

type alias ApiLoginStatus =
  LoginStatus
    { blueskyHandle : Maybe String
    , googleEmail : Maybe String
    , userId : UserId
    }

type alias Model =
  { errors : List { id: Int, msg : String }
  , nextErrorId : Int
  , navKey : Nav.Key
  , latestPrivacyPolicy : Maybe String
  , myPrivacyPolicy : Maybe String
  , page : Page
  , apiLoggedIn : ApiLoginStatus
  , facebookEnabled : Bool
  , facebookLoggedIn : LoginStatus { userId : FacebookId, accessToken : String }
  , facebookUsers : Dict FacebookId Ports.FacebookUser
  , facebookFriends : Maybe (List Ports.FacebookUser)
  , googleEnabled : Bool
  , blueskyLoginHandle : String
  , profiles : Dict UserId Profile
  , wouldsById : Dict WouldId Would
  , columns : List WouldId
  , myBio : String
  , myVisibility : Maybe Audience
  , showMe : Maybe Audience
  , nameSearch : SearchWords.Model
  , bioSearch : SearchWords.Model
  , youWouldChange : Dict UserId (Dict WouldId Bool)
  , pendingYouWould : Dict UserId (Dict WouldId Bool)
  , myNewWould : String
  , drag : Maybe Drag
  }

type OneMsg
  = AddError String
  | DismissError { id : Int }
  | UrlReq { internal : Bool, url : String }
  | SetPage Page
  | FromJS Ports.Update
  | ConnectWebsocket
  | SendWebsocket Json.Decode.Value
  | StartFacebookLogin
  | SetBlueskyLoginHandle String
  | StartLogout
  | CheckApiLogin
  | ApiLoginResult ApiLoginStatus
  | SetDeleteConfirm { id : String, setTo : Bool }
  | DeleteAccount
  | MyPrivacyPolicyVersion String
  | AgreeToPrivacyPolicy { version : String }
  | UpdateUnsubEmail String
  | SendUnsubRequest
  | UnsubscribeCompleted
  | SetWoulds (Dict WouldId Would)
  | SetColumns (List WouldId)
  | EditProposedWould String
  | ChangeWoulds WouldUpdate
  | GotProfile Profile
  | EditBio String
  | SubmitBio
  | MyVisibility Audience
  | SubmitVisibility Audience
  | ShowMe Audience
  | SubmitShowMe Audience
  | NameSearchMsg SearchWords.OutMsg
  | BioSearchMsg SearchWords.OutMsg
  | ProposeYouWould { withId : UserId, wouldId : WouldId, changeTo : Bool }
  | SubmitYouWould
  | ResolveYouWould { withId : UserId, wouldId : WouldId }
  | DragStart Draggable
  | DragEnd
  | DragHover DragTarget
  | DragDrop DragTarget

type alias Msg = List OneMsg

parseUrl : Url -> Page
parseUrl url =
  let
    parser =
      Url.Parser.oneOf
        [ Url.Parser.map Root Url.Parser.top
        , Url.Parser.map Columns (Url.Parser.s "columns")
        , Url.Parser.map accountPage (Url.Parser.s "account")
        , Url.Parser.map Privacy (Url.Parser.s "privacy")
        , Url.Parser.s "unsubscribe"
            <?> Url.Parser.Query.map2
                (\a t -> Unsubscribe
                  { address = a, token = t, success = False, requestUnsubAddress = "" }
                )
                (Url.Parser.Query.string "address")
                (Url.Parser.Query.string "token")
        , Url.Parser.map Security (Url.Parser.s "security")
        , Url.Parser.map WhyNotFacebook (Url.Parser.s "why-not-facebook")
        , Url.Parser.map Test (Url.Parser.s "test")
        ]
  in
  Url.Parser.parse parser url
  |> Maybe.withDefault PageNotFound

onUrlRequest : Browser.UrlRequest -> Msg
onUrlRequest urlReq =
  case urlReq of
    Browser.Internal url -> [UrlReq { internal = True, url = Url.toString url }]
    Browser.External url -> [UrlReq { internal = False, url = url }]

onUrlChange : Url -> Msg
onUrlChange url = [SetPage (parseUrl url)]

init
  : { latestPrivacyPolicy : Maybe String, facebookEnabled : Bool, googleEnabled : Bool }
  -> Url -> Nav.Key -> (Model, Cmd Msg)
init { latestPrivacyPolicy, facebookEnabled, googleEnabled } url navKey =
  let
    initPage = parseUrl url
  in
  ( { errors = []
    , nextErrorId = 0
    , navKey = navKey
    , latestPrivacyPolicy = latestPrivacyPolicy
    , myPrivacyPolicy = Nothing
    , page = initPage
    , apiLoggedIn = Unknown
    , facebookEnabled = facebookEnabled
    , facebookLoggedIn = Unknown
    , facebookUsers = Dict.empty
    , facebookFriends = Nothing
    , googleEnabled = googleEnabled
    , blueskyLoginHandle = ""
    , profiles = Dict.empty
    , wouldsById = Dict.empty
    , columns = []
    , myBio = ""
    , myVisibility = Nothing
    , showMe = Nothing
    , nameSearch = SearchWords.init { htmlInputId = "nameSearch" }
    , bioSearch = SearchWords.init { htmlInputId = "bioSearch" }
    , youWouldChange = Dict.empty
    , pendingYouWould = Dict.empty
    , myNewWould = ""
    , drag = Nothing
    }
  , Cmd.batch
      [ checkApiLogin
      , case initPage of
          Unsubscribe { address, token } ->
            case (address, token) of
              (Just a, Just t) ->
                graphQL
                  { query =
                      [ "mutation U($a:String!,$t:UUID!){"
                      , "completeUnsub(input:{emailAddress:$a,unsubToken:$t}){unit}"
                      , "}"
                      ] |> String.concat
                  , operationName = "U"
                  , variables = [("a", Json.Encode.string a), ("t", Json.Encode.string t)]
                  , decodeResult =
                      Json.Decode.at
                        ["data", "completeUnsub", "unit"]
                        (Json.Decode.succeed [UnsubscribeCompleted])
                  }
              _ -> Cmd.none
          _ -> Cmd.none
      ]
  )

handleHttpResult : Result Http.Error Msg -> Msg
handleHttpResult r =
  case r of
    Err e ->
      let
        errorString = case e of
          Http.BadUrl s -> "BadUrl " ++ s
          Http.Timeout -> "Timeout"
          Http.NetworkError -> "NetworkError"
          Http.BadStatus s -> "BadStatus " ++ String.fromInt s
          Http.BadBody s -> "BadBody " ++ s
      in
      [AddError errorString]
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
    decodeUserId =
      Json.Decode.at
        ["data", "getOrCreateUserId", "userId"]
        (Json.Decode.nullable Json.Decode.string)
    decodeBlueskyHandle =
      Json.Decode.at
        ["data", "getOrCreateUserId", "query", "getBlueskyHandle"]
        (Json.Decode.nullable Json.Decode.string)
    decodeGoogleEmail =
      Json.Decode.at
        ["data", "getOrCreateUserId", "query", "getGoogleEmail"]
        (Json.Decode.nullable Json.Decode.string)
    decodeMyPrivacyPolicy =
      Json.Decode.at
        ["data", "getOrCreateUserId", "query", "myUser"]
        (Json.Decode.nullable (
          Json.Decode.field "privacyPolicyVersion"
            (Json.Decode.nullable Json.Decode.string)
        ))
      |> Json.Decode.map (\m ->
        case m of
          Just (Just v) -> Just (MyPrivacyPolicyVersion v)
          _ -> Nothing
      )
    decodeResult =
      Json.Decode.map4
        (\nu nbsh nge nppv ->
          [ Just << ApiLoginResult <| case nu of
              Nothing -> NotLoggedIn
              Just u ->
                LoggedIn
                  { userId = u
                  , blueskyHandle = nbsh
                  , googleEmail = nge
                  }
          , nppv
          ] |> List.filterMap identity
        )
        decodeUserId
        decodeBlueskyHandle
        decodeGoogleEmail
        decodeMyPrivacyPolicy
  in
  graphQL
    { query =
        [ "mutation L{"
        , "getOrCreateUserId(input:{}){"
        , "userId query{getBlueskyHandle getGoogleEmail myUser{privacyPolicyVersion}}"
        , "}"
        , "}"
        ] |> String.concat
    , operationName = "L"
    , variables = []
    , decodeResult = decodeResult
    }

apiLogin : { accessToken : String } -> Cmd Msg
apiLogin { accessToken } =
  let
    loggedIn () = [CheckApiLogin]
  in
  Http.post
    { url = "/auth/login/facebook"
    , body =
        [ ("userToken", Json.Encode.string accessToken) ]
        |> Json.Encode.object |> Http.jsonBody
    , expect = Http.expectWhatever (handleHttpResult << Result.map loggedIn)
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
  case
    ( ( model.myPrivacyPolicy
      , model.myVisibility
      )
    , ( model.apiLoggedIn
      , model.facebookFriends
      )
    ) of
    ((Just _, Just Friends), (LoggedIn _, Just friends)) ->
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
  "fragment F on UserProfile{userId facebookIds blueskyHandles hasGoogle name bio picture audience friendsSince createdAt matchedWoulds youWould}"

decodeWouldStats : Json.Decode.Decoder (Dict WouldId Would)
decodeWouldStats =
  let
    intAsString =
      Json.Decode.string
      |> Json.Decode.andThen (\s ->
        case String.toInt s of
          Nothing -> Json.Decode.fail <| "expected string containing int, found " ++ s
          Just i -> Json.Decode.succeed i
      )
  in
  Json.Decode.map4
    (\i n a u -> (i, { name = n, addedById = a, uses = u }))
    (Json.Decode.field "wouldId" Json.Decode.string)
    (Json.Decode.field "name" Json.Decode.string)
    (Json.Decode.field "addedById" (Json.Decode.nullable Json.Decode.string))
    (Json.Decode.field "uses" (Json.Decode.nullable intAsString))
  |> Json.Decode.list
  |> Json.Decode.map Dict.fromList

getProfiles
  :  { getMyAudiences : Bool, getWoulds : Bool, userId: UserId }
  -> Model -> Cmd Msg
getProfiles { getMyAudiences, getWoulds, userId } model =
  let
    decodeProfiles =
      Json.Decode.list decodeProfile
      |> Json.Decode.map (List.map GotProfile)
    decodeAudiences =
      Json.Decode.at ["data", "myUser"] (
        Json.Decode.map2 (\msg1 msg2 -> [msg1, msg2])
          (Json.Decode.field "visibleTo" (
            Json.Decode.map MyVisibility decodeAudience
          ))
          (Json.Decode.field "showMe" (
            Json.Decode.map ShowMe decodeAudience
          ))
      )
    decodeWoulds =
      Json.Decode.map2 (\ws cs -> [SetWoulds ws, SetColumns cs])
        (Json.Decode.at ["data", "wouldStats", "nodes"] decodeWouldStats)
        (Json.Decode.at ["data", "getMyColumns"] (Json.Decode.list Json.Decode.string))
  in
  graphQL
    { query =
        [ profileFragment
        , "query Q($u:BigInt!){"
          , "me:userProfiles(condition:{userId:$u}){nodes{...F}}"
          , let
              condition = case model.showMe of
                Just Friends -> "(condition:{audience:FRIENDS})"
                Just Everyone -> ""
                _ ->
                  -- doesn't really make sense, but shouldn't matter too much
                  "(condition:{userId:$u})"
            in
            "them:userProfiles" ++ condition ++ "{nodes{...F}}"
          , if getMyAudiences then "myUser{showMe visibleTo}" else ""
          , if getWoulds then "wouldStats{nodes{wouldId name addedById uses}}getMyColumns" else ""
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
          (if getMyAudiences then decodeAudiences else Json.Decode.succeed [])
          (if getWoulds then decodeWoulds else Json.Decode.succeed [])
    }

setColumns : List WouldId -> Cmd Msg
setColumns cols =
  graphQL
    { query =
      String.concat
        [ "mutation M($c:[BigInt!]!){setMyColumns(input:{columns:$c}){"
          , "query{wouldStats{nodes{wouldId name addedById uses}}}"
        , "}}"
        ]
    , operationName = "M"
    , variables = [("c", Json.Encode.list Json.Encode.string cols)]
    , decodeResult =
        Json.Decode.at ["data", "setMyColumns", "query", "wouldStats", "nodes"] decodeWouldStats
        |> Json.Decode.map (\ws -> [SetWoulds ws])
    }

updateOne : OneMsg -> Model -> (Model, Cmd Msg)
updateOne msg model =
  case msg of
    AddError err ->
      ( { model
        | errors = { id = model.nextErrorId, msg = err } :: model.errors
        , nextErrorId = model.nextErrorId + 1
        }
      , Cmd.none
      )
    DismissError { id } ->
      ( { model | errors = List.filter (\e -> e.id /= id) model.errors }
      , Cmd.none
      )
    UrlReq { internal, url } ->
      if internal
      then (model, Nav.pushUrl model.navKey url)
      else (model, Nav.load url)
    SetPage newPage -> ({ model | page = newPage }, Cmd.none)
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
    FromJS (Ports.FacebookFriends data) ->
      let
        addUser user acc = Dict.insert user.id user acc
        newUsers = List.foldr addUser model.facebookUsers data
        newModel = { model | facebookFriends = Just data, facebookUsers = newUsers }
      in
      (newModel, sendFriends newModel)
    FromJS (Ports.FacebookGotUser user) ->
      ( { model | facebookUsers = Dict.insert user.id user model.facebookUsers }
      , case model.facebookLoggedIn of
          LoggedIn { userId } ->
            if userId == user.id
            then
              graphQL
                { query = "mutation M($n:String!){updateMe(input:{name:$n}){clientMutationId}}"
                , operationName = "M"
                , variables = [("n", Json.Encode.string user.name)]
                , decodeResult = Json.Decode.at ["data", "updateMe"] (Json.Decode.succeed [])
                }
            else Cmd.none
          _ -> Cmd.none
      )
    FromJS (Ports.InProgress continue) -> (model, Cmd.map (\() -> []) continue)
    FromJS Ports.WebsocketClosed ->
      ( model
      , Process.sleep 1000
        |> Task.perform (always [ConnectWebsocket])
      )
    FromJS (Ports.FromWebsocket Ports.ConnectionAck) ->
      ( model
      , Json.Encode.object
          [ ("id", Json.Encode.string "main")
          , ("type", Json.Encode.string "subscribe")
          , ( "payload"
            , Json.Encode.object
                [ ( "query"
                  , [ profileFragment
                    , "subscription S{userUpdate{profile{...F}}}"
                    ] |> String.concat >> Json.Encode.string
                  )
                , ("operationName", Json.Encode.string "S")
                ]
            )
          ]
        |> Ports.sendWebsocket
      )
    FromJS (Ports.FromWebsocket (Ports.Next { payload })) ->
      let
        decodePayload =
            Json.Decode.at ["data", "userUpdate", "profile"] decodeProfile
      in
      ( model
      , Task.perform identity
        << Task.succeed
        <| case Json.Decode.decodeValue decodePayload payload of
          Ok profile -> [GotProfile profile]
          Err error -> [AddError (Json.Decode.errorToString error)]
      )
    ConnectWebsocket -> (model, Ports.connectWebsocket)
    SendWebsocket v -> (model, Ports.sendWebsocket v)
    StartFacebookLogin ->
      case model.facebookLoggedIn of
        LoggingIn -> (model, Cmd.none)
        _ ->
          ( { model | facebookLoggedIn = LoggingIn }
          , Ports.facebookLogin
          )
    SetBlueskyLoginHandle s ->
      ({ model | blueskyLoginHandle = s }, Cmd.none)
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
          , youWouldChange = Dict.empty
          , pendingYouWould = Dict.empty
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
                , url = "/auth/login"
                , body = Http.emptyBody
                , expect =
                    Http.expectWhatever
                      (handleHttpResult << Result.map loggedOut)
                , timeout = Nothing
                , tracker = Nothing
                }
        facebookLogout =
          case (model.facebookEnabled, model.facebookLoggedIn) of
            (False, _) -> Cmd.none
            (_, LoggingOut) -> Cmd.none
            _ -> Ports.facebookLogout
      in
      (newModel, Cmd.batch [apiLogout, facebookLogout])
    CheckApiLogin ->
      (model, checkApiLogin)
    ApiLoginResult newState ->
      let
        newModel = { model | apiLoggedIn = newState }
      in
      case newState of
        LoggedIn { userId } ->
          ( newModel
          , Cmd.batch
              [ getProfiles
                  { getMyAudiences = True, getWoulds = True, userId = userId }
                  newModel
              , sendFriends newModel
              , Ports.connectWebsocket
              ]
          )
        _ -> tryApiLogin newModel
    SetDeleteConfirm { id, setTo } ->
      let
        newPage =
          case model.page of
            Account { deleteConfirmations } ->
              if setTo
              then Account { deleteConfirmations = Set.insert id deleteConfirmations }
              else Account { deleteConfirmations = Set.remove id deleteConfirmations }
            other -> other
      in
      ({ model | page = newPage }, Cmd.none)
    DeleteAccount ->
      ( model
      , graphQL
          { query = "mutation D{deleteMe(input:{}){unit}}"
          , operationName = "D"
          , variables = []
          , decodeResult = Json.Decode.succeed []
          }
      )
    MyPrivacyPolicyVersion v ->
      let
        newModel = { model | myPrivacyPolicy = Just v }
      in
      ( newModel
      , case model.myPrivacyPolicy of
          Just _ -> Cmd.none
          Nothing -> sendFriends newModel
      )
    AgreeToPrivacyPolicy { version } ->
      ( model
      , graphQL
          { query = "mutation P($v:String!){updateMe(input:{privacyPolicyVersion:$v}){user{privacyPolicyVersion}}}"
          , operationName = "P"
          , variables = [("v", Json.Encode.string version)]
          , decodeResult =
              Json.Decode.at ["data", "updateMe", "user", "privacyPolicyVersion"] Json.Decode.string
              |> Json.Decode.map (List.singleton << MyPrivacyPolicyVersion)
          }
      )
    UpdateUnsubEmail newEmail ->
      ( case model.page of
          Unsubscribe u ->
            { model | page = Unsubscribe { u | requestUnsubAddress = newEmail } }
          _ -> model
      , Cmd.none
      )
    SendUnsubRequest ->
      ( model
      , case model.page of
          Unsubscribe { requestUnsubAddress } ->
            graphQL
              { query = "mutation U($a:String!){requestUnsub(input:{emailAddress:$a}){unit}}"
              , operationName = "U"
              , variables = [("a", Json.Encode.string requestUnsubAddress)]
              , decodeResult =
                  Json.Decode.at ["data", "requestUnsub", "unit"]
                    (Json.Decode.succeed [UpdateUnsubEmail ""])
              }
          _ -> Cmd.none
      )
    UnsubscribeCompleted ->
      ( case model.page of
          Unsubscribe u ->
            { model | page = Unsubscribe { u | success = True } }
          _ -> model
      , Cmd.none
      )
    SetWoulds woulds -> ({ model | wouldsById = woulds }, Cmd.none)
    SetColumns cols -> ({ model | columns = cols }, setColumns cols)
    EditProposedWould name -> ({ model | myNewWould = name }, Cmd.none)
    ChangeWoulds change ->
      let
        ((params, variables), (function, args)) = case change of
          CreateWould { name } ->
            ( ("($n:String!)", [("n", Json.Encode.string name)])
            , ("createWould", "(input:{would:{name:$n}})")
            )
          RenameWould { id, name } ->
            ( ( "($i:BigInt!,$n:String!)"
              , [ ("i", Json.Encode.string id)
                , ("n", Json.Encode.string name)
                ]
              )
            , ("updateWould", "(input:{wouldId:$i,patch:{name:$n}})")
            )
          DeleteWould { id } ->
            ( ("($i:BigInt!)", [("i", Json.Encode.string id)])
            , ("deleteWould", "(input:{wouldId:$i})")
            )
      in
      ( model
      , graphQL
          { query =
              String.concat
                [ "mutation C" ++ params ++ "{"
                  , function ++ args ++ "{"
                    , "query{wouldStats{nodes{wouldId name addedById uses}}}"
                    , "would{wouldId}"
                  , "}"
                , "}"
                ]
          , operationName = "C"
          , variables = variables
          , decodeResult =
              Json.Decode.at ["data", function]
              <| Json.Decode.map2
                (\ws wId -> [SetWoulds ws] ++ case change of
                  CreateWould _ -> [SetColumns (model.columns ++ [wId])]
                  _ -> []
                )
                (Json.Decode.at ["query", "wouldStats", "nodes"] decodeWouldStats)
                (Json.Decode.at ["would", "wouldId"] Json.Decode.string)
          }
      )
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
                    Set.member wId user.youWouldIds /= changeTo
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
        , youWouldChange = Dict.update user.userId redundantChange model.youWouldChange
        , myBio = if isMe user.userId then user.bio else model.myBio
        }
      , if isMe user.userId && not (List.isEmpty user.blueskyHandles)
        then
          graphQL
            { query = "mutation M{requestMyBlueskyProfile(input:{}){unit}}"
            , operationName = "M"
            , variables = []
            , decodeResult = Json.Decode.field "data" (Json.Decode.succeed [])
            }
        else Cmd.none
      )
    EditBio bio -> ({ model | myBio = bio }, Cmd.none)
    SubmitBio ->
      ( model
      , case model.apiLoggedIn of
          LoggedIn { userId } ->
            graphQL
              { query =
                  profileFragment
                  ++ "mutation B($b:String!,$u:BigInt!){updateMe(input:{bio:$b}){query{userProfiles(condition:{userId:$u}){nodes{...F}}}}}"
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
    SubmitVisibility who ->
      ( model
      , graphQL
          { query = "mutation V($a:Audience!){updateMe(input:{visibleTo:$a}){user{visibleTo}}}"
          , operationName = "V"
          , variables = [("a", encodeAudience who)]
          , decodeResult =
              Json.Decode.at ["data", "updateMe", "user", "visibleTo"] decodeAudience
              |> Json.Decode.map (List.singleton << MyVisibility)
          }
      )
    ShowMe who ->
      let
        newModel = { model | showMe = Just who }
      in
      ( newModel
      , case model.apiLoggedIn of
          LoggedIn { userId } ->
            getProfiles
              { getMyAudiences = False, getWoulds = False, userId = userId }
              newModel
          _ -> Cmd.none
      )
    SubmitShowMe who ->
      ( model
      , graphQL
          { query = "mutation V($a:Audience!){updateMe(input:{showMe:$a}){user{showMe}}}"
          , operationName = "V"
          , variables = [("a", encodeAudience who)]
          , decodeResult =
              Json.Decode.at ["data", "updateMe", "user", "showMe"] decodeAudience
              |> Json.Decode.map (List.singleton << ShowMe)
          }
      )
    NameSearchMsg nsMsg ->
      let
        (newNameSearch, cmd) = SearchWords.update model.nameSearch nsMsg
      in
      ( { model | nameSearch = newNameSearch }
      , Cmd.map (List.map NameSearchMsg) cmd
      )
    BioSearchMsg bsMsg ->
      let
        (newBioSearch, cmd) = SearchWords.update model.bioSearch bsMsg
      in
      ( { model | bioSearch = newBioSearch }
      , Cmd.map (List.map BioSearchMsg) cmd
      )
    ProposeYouWould { withId, wouldId, changeTo } ->
      let
        isAlready =
          case Dict.get withId model.profiles of
            Nothing -> False
            Just p -> Set.member wouldId p.youWouldIds == changeTo
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
      ( { model | youWouldChange = Dict.update withId change model.youWouldChange }
      , Cmd.none
      )
    SubmitYouWould ->
      case model.apiLoggedIn of
        LoggedIn { userId } ->
          ( { model | pendingYouWould = model.youWouldChange }
          , Dict.toList model.youWouldChange
            |> List.map (\(uid, woulds) ->
                Dict.toList woulds
                |> List.map (\(wId, changeTo) ->
                    graphQL
                      { query =
                          [ "mutation C($u:BigInt!,$wo:BigInt!,$wi:BigInt!){"
                          , if changeTo
                            then "createUserWould(input:{userWould:"
                            else "deleteUserWould(input:"
                          , "{userId:$u,wouldId:$wo,withId:$wi}"
                          , if changeTo
                            then "})"
                            else ")"
                          , "{userWould{nodeId}}"
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
                            , "userWould"
                            ]
                            (Json.Decode.succeed ())
                          |> Json.Decode.map (\() -> [ResolveYouWould { withId = uid, wouldId = wId }])
                      }
                  )
              )
            |> List.concat
            |> Cmd.batch
          )
        _ -> (model, Cmd.none)
    ResolveYouWould { withId, wouldId } ->
      let
        removeWould md = case md of
          Nothing -> Nothing
          Just d ->
            let
              new = Dict.remove wouldId d
            in
            if Dict.isEmpty new then Nothing else Just new
        newPending = Dict.update withId removeWould model.pendingYouWould
        newProposed = Dict.update withId removeWould model.youWouldChange
      in
      ( { model | pendingYouWould = newPending, youWouldChange = newProposed }
      , if Dict.isEmpty newPending
        then
          case model.apiLoggedIn of
            LoggedIn api ->
              getProfiles
                { getMyAudiences = False, getWoulds = False, userId = api.userId }
                model
            _ -> Cmd.none
        else Cmd.none
      )
    DragStart draggable ->
      ( { model | drag = Just { held = draggable, over = Nothing } }
      , Cmd.none
      )
    DragEnd ->
      ( { model | drag = Nothing }
      , Cmd.none
      )
    DragHover target ->
      ( { model | drag = model.drag |> Maybe.map (\d -> { d | over = Just target }) }
      , Cmd.none
      )
    DragDrop (Column target) ->
      case model.drag of
        Nothing -> (model, Cmd.none)
        Just { held } ->
          case held of
            Column heldId ->
              let
                newColumns =
                  if target == heldId
                  then model.columns
                  else case ListZipper.findFirst (\_ c -> c == heldId) model.columns of
                    Nothing -> model.columns
                    Just (_, { before, after }) ->
                      let
                        insertAfter x = if x == target then [x, heldId] else [x]
                      in
                      ListZipper.toList
                        { before = List.concatMap insertAfter before
                        , after = List.concatMap insertAfter after
                        }
              in
              ( { model | drag = Nothing, columns = newColumns }
              , setColumns newColumns
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
subscriptions model =
  let
    translateMsg msg =
      case msg of
        Ok fromJS -> [FromJS fromJS]
        Err (Ports.DriverProtocolError s) -> [AddError s]
        Err (Ports.SDKLoadFailed Ports.Facebook) ->
          [ AddError """Facebook SDK failed to load. If you want to use Facebook
              features but your browser blocks Facebook tracking, see if you can
              enable it for this page specifically."""
          ]
        Err (Ports.FacebookAccessTokenExpired { whileDoing }) ->
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
          then []
          else [AddError ("Facebook auth failed for " ++ whileDoing)]
        Err (Ports.FacebookUnknownError { whileDoing, error }) ->
          [AddError ("Error (" ++ whileDoing ++ "): " ++ Json.Encode.encode 0 error)]
  in
  Sub.map translateMsg Ports.subscriptions
