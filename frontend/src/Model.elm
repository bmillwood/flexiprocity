module Model exposing (..)

import Browser
import Browser.Navigation as Nav
import Dict exposing (Dict)
import Http
import Json.Decode
import Json.Encode
import Set exposing (Set)
import Url exposing (Url)
import Url.Parser

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
  , facebookId : String
  , name : Maybe String
  , bio : String
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
  Json.Decode.map2
    identity
    (Json.Decode.map8
      Profile
      (Json.Decode.field "userId" Json.Decode.string)
      (Json.Decode.field "facebookId" Json.Decode.string)
      (Json.Decode.field "name" (Json.Decode.nullable Json.Decode.string))
      (Json.Decode.field "bio" Json.Decode.string)
      (Json.Decode.field "audience" decodeAudience)
      (Json.Decode.field "friendsSince" (Json.Decode.nullable Json.Decode.string))
      (Json.Decode.field "createdAt" Json.Decode.string)
      (Json.Decode.field "matchedWoulds" decodeWouldIds))
    (Json.Decode.field "youWould" decodeWouldIds)

type Page
  = PageNotFound
  | Root
  | Columns
  | Account { deleteConfirmations : Set String }
  | Privacy
  | Security

accountPage : Page
accountPage = Account { deleteConfirmations = Set.empty }

type alias Would = { name : String, uses : Maybe Int }
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

type alias Model =
  { errors : List { id: Int, msg : String }
  , nextErrorId : Int
  , navKey : Nav.Key
  , latestPrivacyPolicy : Maybe String
  , myPrivacyPolicy : Maybe String
  , page : Page
  , facebookLoggedIn : LoginStatus { userId : FacebookId, accessToken : String }
  , apiLoggedIn : LoginStatus { userId : UserId }
  , facebookUsers : Dict FacebookId Ports.FacebookUser
  , profiles : Dict UserId Profile
  , facebookFriends : Maybe (List Ports.FacebookUser)
  , wouldsById : Dict WouldId Would
  , columns : List WouldId
  , myBio : String
  , myVisibility : Maybe Audience
  , showMe : Audience
  , nameSearch : SearchWords.Model
  , bioSearch : SearchWords.Model
  , wouldChange : Dict UserId (Dict WouldId Bool)
  , myNewWould : String
  , drag : Maybe Drag
  }

type OneMsg
  = AddError String
  | DismissError { id : Int }
  | UrlReq { internal : Bool, url : String }
  | SetPage Page
  | FromJS Ports.Update
  | StartFacebookLogin
  | StartLogout
  | CheckApiLogin
  | ApiLoginResult (LoginStatus { userId : UserId })
  | SetDeleteConfirm { id : String, setTo : Bool }
  | DeleteAccount
  | MyPrivacyPolicyVersion String
  | AgreeToPrivacyPolicy { version : String }
  | SetWoulds (Dict WouldId Would)
  | SetColumns (List WouldId)
  | EditProposedWould String
  | ChangeWoulds WouldUpdate
  | GotProfile Profile
  | EditBio String
  | SubmitBio
  | MyVisibility Audience
  | SubmitVisibility
  | ShowMe Audience
  | NameSearchMsg SearchWords.OutMsg
  | BioSearchMsg SearchWords.OutMsg
  | WouldChange { userId : UserId, wouldId : WouldId, changeTo : Bool }
  | SubmitWouldChanges
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
        , Url.Parser.map Security (Url.Parser.s "security")
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

init : { latestPrivacyPolicy : Maybe String } -> Url -> Nav.Key -> (Model, Cmd Msg)
init { latestPrivacyPolicy } url navKey =
  ( { errors = []
    , nextErrorId = 0
    , navKey = navKey
    , latestPrivacyPolicy = latestPrivacyPolicy
    , myPrivacyPolicy = Nothing
    , page = parseUrl url
    , facebookLoggedIn = Unknown
    , apiLoggedIn = Unknown
    , facebookUsers = Dict.empty
    , profiles = Dict.empty
    , facebookFriends = Nothing
    , wouldsById = Dict.empty
    , columns = []
    , myBio = ""
    , myVisibility = Nothing
    , showMe = Friends
    , nameSearch = SearchWords.init { htmlInputId = "nameSearch" }
    , bioSearch = SearchWords.init { htmlInputId = "bioSearch" }
    , wouldChange = Dict.empty
    , myNewWould = ""
    , drag = Nothing
    }
  , checkApiLogin
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
      |> Json.Decode.map (Maybe.map (\u -> { userId = u }))
    decodeMyPrivacyPolicy =
      Json.Decode.at
        ["data", "getOrCreateUserId", "query", "myUser", "privacyPolicyVersion"]
        (Json.Decode.nullable Json.Decode.string)
      |> Json.Decode.map (Maybe.map MyPrivacyPolicyVersion)
    decodeResult =
      Json.Decode.map2
        (\nu nppv ->
          [ Just << ApiLoginResult <| case nu of
              Nothing -> NotLoggedIn
              Just u -> LoggedIn u
          , nppv
          ] |> List.filterMap identity
        )
        decodeUserId
        decodeMyPrivacyPolicy
  in
  graphQL
    { query =
        [ "mutation L{"
        , "getOrCreateUserId(input:{}){"
        , "userId query{myUser{privacyPolicyVersion}}"
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
  "fragment F on UserProfile{userId facebookId name bio audience friendsSince createdAt matchedWoulds youWould}"

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
  Json.Decode.map3
    (\i n u -> (i, { name = n, uses = u }))
    (Json.Decode.field "wouldId" Json.Decode.string)
    (Json.Decode.field "name" Json.Decode.string)
    (Json.Decode.field "uses" (Json.Decode.nullable intAsString))
  |> Json.Decode.list
  |> Json.Decode.map Dict.fromList

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
                Self ->
                  -- doesn't really make sense, but we don't use this anyway
                  "(condition:{userId:$u})"
                Friends -> "(condition:{audience:FRIENDS})"
                Everyone -> ""
            in
            "them:userProfiles" ++ condition ++ "{nodes{...F}}"
          , if getMyVisibility then "myUser{visibleTo}" else ""
          , if getWoulds then "wouldStats{nodes{wouldId name uses}}getMyColumns" else ""
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

setColumns : List WouldId -> Cmd Msg
setColumns cols =
  graphQL
    { query =
      String.concat
        [ "mutation M($c:[BigInt!]!){setMyColumns(input:{columns:$c}){"
          , "query{wouldStats{nodes{wouldId name uses}}}"
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
                , url = "/auth/login/facebook"
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
      ({ model | myPrivacyPolicy = Just v }, Cmd.none)
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
            ( ( "($i:String!,$n:String!)"
              , [ ("i", Json.Encode.string id)
                , ("n", Json.Encode.string name)
                ]
              )
            , ("updateWould", "(input:{wouldId:$i,patch:{name:$n}})")
            )
          DeleteWould { id } ->
            ( ("($i:String!)", [("i", Json.Encode.string id)])
            , ("deleteWould", "(input:{wouldId:$i})")
            )
      in
      ( model
      , graphQL
          { query =
              String.concat
                [ "mutation C" ++ params ++ "{"
                  , function ++ args ++ "{"
                    , "query{wouldStats{nodes{wouldId name uses}}}"
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
    WouldChange { userId, wouldId, changeTo } ->
      let
        isAlready =
          case Dict.get userId model.profiles of
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
        Err Ports.FacebookSDKLoadFailed ->
          [ AddError """Facebook SDK failed to load. This app (for now)
              requires Facebook login in order to function. If your
              browser blocks Facebook tracking, see if you can enable
              it for this page specifically."""
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
