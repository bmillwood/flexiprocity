module View exposing (view)

import Browser
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events
import Json.Decode
import Set exposing (Set)

import Model exposing (Model, Msg)
import PrivacyPolicy
import SearchWords

profileName : Model -> Model.Profile -> Maybe String
profileName model profile =
  case Dict.get profile.facebookId model.facebookUsers of
    Nothing -> profile.name
    Just { name } -> Just name

viewUser : Model -> Model.Profile -> { isMe : Bool } -> Html Msg
viewUser model user { isMe } =
  let
    facebookUser = Dict.get user.facebookId model.facebookUsers
    name =
      profileName model user
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
            , Attributes.style "margin" "0.2em 0.4em"
            ]
            []
    , Html.div
        []
        [ let
            linkName nameHtml =
              case facebookUser |> Maybe.andThen .link of
                Nothing -> nameHtml
                Just link ->
                  [ Html.a
                      [ Attributes.href link ]
                      nameHtml
                  ]
            highlightedName =
              if isMe
              then [ Html.text name ]
              else SearchWords.highlightMatches model.nameSearch name
          in
          Html.div
            [ Attributes.style "font-weight" "bold" ]
            (linkName highlightedName)
        , Html.div
            [ Attributes.style "margin" "0.1em 0.5em"
            , Attributes.style "font-size" "90%"
            ]
            (
            if isMe
            then
              [ Html.form
                  [ Events.onSubmit [Model.SubmitBio] ]
                  [ Html.input
                      [ Attributes.type_ "text"
                      , Attributes.placeholder "short bio"
                      , Attributes.value model.myBio
                      , Events.onInput (List.singleton << Model.EditBio)
                      ]
                      []
                  , let
                      saved = user.bio == model.myBio
                    in
                    Html.button
                      [ Events.onClick [Model.SubmitBio]
                      , Attributes.disabled saved
                      , Attributes.style "margin-left" "0.2em"
                      ]
                      [ Html.text (if saved then "Saved" else "Save") ]
                  ]
              ]
            else
              SearchWords.highlightMatches model.bioSearch user.bio
          )
        ]
    ]

viewCustomiseColumns : Model -> { myUserId : Model.UserId } -> List (Html Msg)
viewCustomiseColumns model { myUserId } =
  let
    indexInOrLength x xs =
      List.foldr (\this acc -> if x == this then 0 else 1 + acc) 0 xs
    columnChoice index (wId, would) =
      let
        checkboxId = "column-" ++ wId
        isChecked = List.member wId model.columns
        onCheck nowChecked =
          [ Model.SetColumns (
              if nowChecked
              then model.columns ++ [wId]
              else List.filter (\c -> c /= wId) model.columns
            )
          ]
        dragAttributes =
          [ Attributes.draggable "true"
          , Events.on "dragstart" (
              Json.Decode.succeed [Model.DragStart (Model.Column wId)]
            )
          , Events.preventDefaultOn "dragover" (
              Json.Decode.succeed ([Model.DragHover (Model.Column wId)], True)
            )
          , Events.on "drop" (
              Json.Decode.succeed [Model.DragDrop (Model.Column wId)]
            )
          , Events.on "dragend" (
              Json.Decode.succeed [Model.DragEnd]
            )
          ] ++ case model.drag of
            Nothing -> []
            Just { held, over } -> case (held, over) of
              (_, Nothing) -> []
              (Model.Column heldId, Just (Model.Column hoverId)) ->
                if wId == hoverId
                then
                  [ Attributes.class
                    <| case compare index (indexInOrLength heldId model.columns) of
                      LT -> "insertAbove"
                      GT -> "insertBelow"
                      EQ -> "insertUnmoved"
                  ]
                else []
      in
      Html.li
        (if isChecked then dragAttributes else [])
        [ Html.label
            [ Attributes.for checkboxId ]
            (List.concat
              [ if isChecked
                then
                  [ Html.span
                      [ Attributes.style "cursor" "grab" ]
                      [ Html.text "::" ]
                  ]
                else []
              , [ Html.input
                    [ Attributes.type_ "checkbox"
                    , Attributes.id checkboxId
                    , Attributes.checked isChecked
                    , Events.onCheck onCheck
                    ]
                    []
                ]
              , case would.uses of
                  Nothing -> []
                  Just uses ->
                    [ Html.span
                        [ Attributes.style "font-size" "50%"
                        , Attributes.style "opacity" "0.5"
                        ]
                        [ Html.text "×"
                        , Html.text (String.fromInt uses)
                        , Html.text " "
                        ]
                    ]
              , [ Html.text would.name ]
              , let
                  canDelete =
                    would.addedById == Just myUserId
                    && case would.uses of
                      -- usually this means "default" which also means "no
                      -- addedById", so this case should be unreachable
                      Nothing -> False
                      -- this is not the real condition but it's close
                      Just n -> n <= (if isChecked then 1 else 0)
                in
                if canDelete
                then
                  [ Html.button
                      [ Attributes.style "font-size" "50%"
                      , Events.onClick [Model.ChangeWoulds (Model.DeleteWould { id = wId })]
                      ]
                      [ Html.text "❌" ]
                  ]
                else []
              ]
            )
        ]
    lexOrder o1 o2 = if o1 == EQ then o2 else o1
    compareWoulds (wId1, would1) (wId2, would2) =
      List.foldr lexOrder EQ
        [ compare
            (indexInOrLength wId1 model.columns)
            (indexInOrLength wId2 model.columns)
        , case (would1.uses, would2.uses) of
            (Nothing, Nothing) -> EQ
            (Nothing, Just _) -> LT
            (Just _, Nothing) -> GT
            (Just u1, Just u2) -> compare u2 u1
        , compare would1.name would2.name
        , compare wId1 wId2
        ]
    columnChoices =
      Dict.toList model.wouldsById
      |> List.sortWith compareWoulds
      |> List.indexedMap columnChoice
    createNewColumn =
      let
        submit =
          [ Model.ChangeWoulds (Model.CreateWould { name = model.myNewWould })
          , Model.EditProposedWould ""
          ]
      in
      [ Html.form
          [ Events.onSubmit submit ]
          [ Html.input
              [ Attributes.type_ "text"
              , Attributes.placeholder "create new (max 3/day)"
              , Attributes.value model.myNewWould
              , Events.onInput (List.singleton << Model.EditProposedWould)
              ]
              []
          ]
      ]
  in
  [ Html.div
      [ Attributes.style "position" "sticky"
      , Attributes.style "bottom" "0"
      , Attributes.style "padding" "0.2em"
      , Attributes.class "customiseColumns"
      ]
      [ Html.ul
          [ Attributes.style "list-style-type" "none" ]
          (columnChoices ++ [ Html.li [] createNewColumn ])
      ]
  ]

viewPeople : { customiseColumns : Bool, myUserId : Model.UserId } -> Model -> List (Html Msg)
viewPeople { customiseColumns, myUserId } model =
  [ Html.p []
      [ case (model.facebookLoggedIn, model.facebookFriends) of
          (_, Just friends) ->
            if List.isEmpty friends
            then Html.text "None of your Facebook friends use reciprocity 🙁"
            else
              [ String.fromInt (List.length friends)
              , " of your Facebook friends use reciprocity"
              ] |> String.concat |> Html.text
          (Model.LoggedIn _, Nothing) ->
            Html.text "Retrieving your Facebook friends..."
          _ ->
            Html.span
              [ Attributes.class "error" ]
              [ Html.text "Log in with Facebook to see pictures, profile links etc." ]
      ]
  , Html.table []
      [ Html.tr []
          [ Html.td [] [Html.text "Search names: "]
          , Html.td []
              [ SearchWords.view model.nameSearch
                |> Html.map (List.map Model.NameSearchMsg)
              ]
          ]
      , Html.tr []
          [ Html.td [] [Html.text "Search bios: "]
          , Html.td []
              [ SearchWords.view model.bioSearch
                |> Html.map (List.map Model.BioSearchMsg)
              ]
          ]
      ]
  , let
      showClass cl =
        Html.span
          [ Attributes.class cl
          , Attributes.style "padding" "0 0.2em"
          ]
          [ Html.text cl ]
    in
    Html.div
      [ Attributes.style "text-align" "right" ]
      [ showClass "modified"
      , Html.text " "
      , showClass "submitted"
      , Html.text " "
      , showClass "matched"
      ]
  , let
      colsById =
        List.filterMap
          (\id ->
            Dict.get id model.wouldsById
            |> Maybe.map (\v -> (id, v.name))
          )
          model.columns
    in
    Html.table
      [ Attributes.style "width" "100%"
      , Attributes.style "padding" "1em"
      , Attributes.style "border-collapse" "collapse"
      , Attributes.id "people"
      ]
      [ Html.thead
          [ Attributes.style "position" "sticky"
          , Attributes.style "top" "0"
          ]
          [ let
              colWidth =
                String.fromInt (min 10 (round (50 / toFloat (List.length colsById)))) ++ "%"
              peopleHeaderCol =
                Html.th
                  [ Attributes.style "text-align" "left"
                  , Attributes.style "padding-left" "1em"
                  ]
                  [ Html.div
                      [ Attributes.style "display" "flex"
                      , Attributes.style "justify-content" "space-between"
                      ]
                      [ Html.span [] [ Html.text "People" ]
                      , Html.span []
                          [ Html.button
                              [ Events.onClick
                                  [ Model.UrlReq
                                      { internal = True
                                      , url =
                                          if customiseColumns
                                          then "/"
                                          else "/columns"
                                      }
                                  ]
                              ]
                              [ if customiseColumns
                                then Html.text "Done customising"
                                else Html.text "Customise"
                              ]
                          ]
                      ]
                  ]
              wouldHeaderCol (_, wName) =
                Html.th
                  [ Attributes.style "width" colWidth ]
                  [ Html.text wName ]
              cols =
                peopleHeaderCol ::
                if List.isEmpty colsById
                then
                  [ Html.td
                      [ Attributes.style "text-align" "center"
                      , Attributes.style "font-style" "italic"
                      , Attributes.style "width" "10%"
                      ]
                      [ Html.text "choose some columns" ]
                  ]
                else List.map wouldHeaderCol colsById
            in
            Html.tr [] cols
          , Html.tr
              []
              [ Html.th [] []
              , Html.th
                  [ Attributes.colspan (List.length colsById) ]
                  [ Html.button
                      [ Events.onClick [Model.SubmitYouWould]
                      , Attributes.disabled (Dict.isEmpty model.youWouldChange)
                      ]
                      [ Html.text "Submit" ]
                  ]
              ]
          ]
      , let
          viewProfile profile =
            let
              toWouldNames ids =
                Set.toList ids
                |> List.filterMap (\i ->
                  Dict.get i model.wouldsById |> Maybe.map .name
                )
                |> Set.fromList
              youWouldNames = toWouldNames profile.youWouldIds
              matchedNames = toWouldNames profile.matchedWouldIds
              wouldCol (wId, wName) =
                let
                  isMatched = Set.member wName matchedNames
                  isYouWould = Set.member wName youWouldNames
                  isModified =
                    Dict.get profile.userId model.youWouldChange
                    |> Maybe.map (Dict.member wId)
                    |> Maybe.withDefault False
                  styles =
                    [ [ Attributes.style "text-align" "center"
                      , Attributes.style "transition" "background-color 0.2s"
                      ]
                    , if isMatched
                      then [ Attributes.class "matched" ]
                      else if isModified
                      then [ Attributes.class "modified" ]
                      else if isYouWould
                      then [ Attributes.class "submitted" ]
                      else []
                    ] |> List.concat
                  onCheck newChecked =
                    [ Model.ProposeYouWould
                        { userId = profile.userId
                        , wouldId = wId
                        , changeTo = newChecked
                        }
                    ]
                  isChecked =
                    case
                      Dict.get profile.userId model.youWouldChange
                      |> Maybe.withDefault Dict.empty
                      |> Dict.get wId
                    of
                      Nothing -> Set.member wName youWouldNames
                      Just b -> b
                  tdAttrs =
                    Events.onClick (onCheck (not isChecked))
                    :: styles
                in
                Html.td
                  tdAttrs
                  [ let
                      checkboxId = "would-" ++ profile.userId ++ "-" ++ wId
                    in
                    Html.label
                      [ Attributes.for checkboxId
                      , Attributes.style "display" "block"
                      ]
                      [ Html.input
                          [ Attributes.type_ "checkbox"
                          , Attributes.id checkboxId
                          , Attributes.disabled isMatched
                          , Attributes.checked isChecked
                          , Events.onCheck onCheck
                          ]
                          []
                      ]
                  ]
              cols =
                Html.td [] [viewUser model profile { isMe = False }]
                :: List.map wouldCol colsById
            in
            Html.tr [] cols
          profileCompare p1 p2 =
            let
              nullsLast m1 m2 =
                case (m1, m2) of
                  (Nothing, Nothing) -> EQ
                  (Nothing, _) -> GT
                  (_, Nothing) -> LT
                  (Just v1, Just v2) -> compare v1 v2
              lexicographic next prev =
                case prev of
                  EQ -> next
                  _ -> prev
            in
            -- swap p1 and p2 when we want descending sort
            [ compare
                (Set.size p2.matchedWouldIds)
                (Set.size p1.matchedWouldIds)
            , compare
                (Set.size p2.youWouldIds)
                (Set.size p1.youWouldIds)
            , compare
                (Maybe.withDefault "" p2.friendsSince)
                (Maybe.withDefault "" p1.friendsSince)
            , compare p2.createdAt p1.createdAt
            , nullsLast (profileName model p1) (profileName model p2)
            ] |> List.foldr lexicographic EQ
          sortProfiles = List.sortWith profileCompare
          filterName profile =
            case profileName model profile of
              Nothing -> not (SearchWords.isActive model.nameSearch)
              Just n -> SearchWords.hasMatch model.nameSearch n
          filterBio profile =
            SearchWords.hasMatch model.bioSearch profile.bio
          filterAudience profile =
            case model.showMe of
              Model.Everyone ->
                profile.audience /= Model.Self
              Model.Friends ->
                profile.audience == Model.Friends
              Model.Self ->
                -- not exposed, but there's a logical thing to do here
                False
          filterProfile profile =
            filterName profile && filterBio profile && filterAudience profile
          profiles =
            Dict.values model.profiles
            |> List.filter filterProfile
            |> sortProfiles
        in
        Html.tbody [] (
          if List.isEmpty profiles
          then
            [ Html.td
                [ Attributes.colspan (List.length colsById + 1)
                , Attributes.style "padding" "1em"
                , Attributes.style "font-style" "italic"
                ]
                [ Html.text "(No users visible)" ]
            ]
          else List.map viewProfile profiles
        )
      ]
  ] ++ if customiseColumns then viewCustomiseColumns model { myUserId = myUserId } else []

viewLogin : Model -> Html Msg
viewLogin model =
  let
    button disabled text =
      Html.button
        [ Events.onClick [Model.StartFacebookLogin]
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
  Html.p []
  <| case model.facebookLoggedIn of
    Model.Unknown ->
      [ button True "Checking Facebook login status" ]
    Model.NotLoggedIn ->
      [ button False "Login with Facebook" ]
    Model.LoggingIn ->
      [ button True "Logging in..." ]
    Model.LoggedIn { userId } ->
      [ Html.span
          [ Attributes.class "facebook-button"
          , Attributes.class "facebook-logged-in"
          ]
          (viewLoggedIn userId)
      , Html.button
          [ Attributes.class "facebook-button"
          , Attributes.class "logout"
          , Events.onClick [Model.StartLogout]
          ]
          [ Html.text "Logout" ]
      ]
    Model.LoggingOut ->
      [ Html.button
          [ Attributes.class "facebook-button"
          , Attributes.class "logout"
          , Attributes.disabled True
          ]
          [ Html.text "Logging out..." ]
      ]

viewAudienceControls : Model -> List (Html Msg)
viewAudienceControls model =
  let
    audienceRadio { name, currentWho, onCheck, who, label } =
      [ Html.input
        [ Attributes.type_ "radio"
        , Attributes.name name
        , Attributes.id (name ++ "-" ++ Model.audienceToString who)
        , Attributes.checked (currentWho == Just who)
        , Events.onCheck (\_ -> onCheck)
        ] []
      , Html.label
          [ Attributes.for (name ++ "-" ++ Model.audienceToString who) ]
          [ Html.text label ]
      ]
  in
  [ let
      radio who label =
        audienceRadio
          { name = "visibility"
          , currentWho = model.myVisibility
          , onCheck = [Model.MyVisibility who, Model.SubmitVisibility]
          , who = who
          , label = label
          }
    in
    [ [ Html.text "Show my profile to people I've ticked and:" ]
    , radio Model.Self "Nobody else"
    , radio Model.Friends "Friends"
    , radio Model.Everyone "Everyone"
    ] |> List.concat |> Html.p []
  , let
      radio who label =
        audienceRadio
          { name = "search"
          , currentWho = Just model.showMe
          , onCheck = [Model.ShowMe who]
          , who = who
          , label = label
          }
    in
    [ [ Html.text "I want to see:" ]
    , radio Model.Friends "My friends"
    , radio Model.Everyone "Everyone"
    ] |> List.concat |> Html.p []
  ]

viewRoot : { customiseColumns : Bool } -> Model -> List (Html Msg)
viewRoot { customiseColumns } model =
  let
    whatIsReciprocity =
      [ Html.p [] [ Html.text "reciprocity offers you a list of other users and some tickboxes for each user." ]
      , Html.p [] [ Html.text "We only tell them you ticked them if they tick you too." ]
      ]
    normalPage { userId } =
      [ viewAudienceControls model
      , case Dict.get userId model.profiles of
          Just u -> [viewUser model u { isMe = True }]
          Nothing -> []
      , viewPeople { customiseColumns = customiseColumns, myUserId = userId } model
      ] |> List.concat
    privacyPrompt =
      Html.p [] [Html.text "Use the nav bar to head to the privacy page and take a look."]
  in
  [ [ viewLogin model ]
  , case model.apiLoggedIn of
      Model.LoggedIn { userId } ->
        case (model.latestPrivacyPolicy, model.myPrivacyPolicy) of
          (Nothing, _) -> normalPage { userId = userId }
          (Just _, Nothing) ->
            [ whatIsReciprocity
            , [ Html.p [] [Html.text "I think you're new here, so you'll need to agree to the privacy policy."]
              , privacyPrompt
              ]
            ] |> List.concat
          (Just latestVersion, Just myVersion) ->
            if latestVersion == myVersion
            then normalPage { userId = userId }
            else
              [ Html.p [] [Html.text "Looks like the privacy policy has been updated since you agreed to it."]
              , privacyPrompt
              ]
      _ -> whatIsReciprocity
  ] |> List.concat

viewPrivacy : Model -> List (Html Msg)
viewPrivacy model =
  let
    updatePrivacy =
      case (model.apiLoggedIn, model.latestPrivacyPolicy) of
        (Model.LoggedIn _, Just latestVersion) ->
          let
            agreeButton text =
              Html.p
                []
                [ Html.button
                    [ Events.onClick [Model.AgreeToPrivacyPolicy { version = latestVersion }] ]
                    [ Html.text text ]
                ]
            needsButton =
              case model.myPrivacyPolicy of
                Nothing -> True
                Just myVersion -> latestVersion /= myVersion
          in
          case model.myPrivacyPolicy of
            Nothing ->
              [ Html.p [] [ Html.text "You have not agreed to the privacy policy. Please read it and agree:" ]
              , agreeButton "Agree"
              ]
            Just myVersion ->
              if latestVersion == myVersion
              then []
              else
                [ Html.p []
                    [ Html.text "You have agreed to an older privacy policy (latest is "
                    , Html.text latestVersion
                    , Html.text ", you have "
                    , Html.text myVersion
                    , Html.text "). I haven't yet worked out how to conveniently show you the changes, but maybe you will find the "
                    , Html.a
                        [ Attributes.href "https://github.com/bmillwood/flexiprocity/commits/main/frontend/src/PrivacyPolicy.elm" ]
                        [ Html.text "changes to the relevant source file" ]
                    , Html.text " helpful."
                    ]
                , agreeButton "Agree to the new policy"
                ]
        _ -> []
  in
  updatePrivacy ++ PrivacyPolicy.viewPrivacyPolicy

viewSecurity : Model -> List (Html Msg)
viewSecurity _ =
  [ Html.h2 []
      [ Html.text "Reporting security issues" ]
  , Html.p []
      [ Html.text "There are two ways to report security flaws in the app:"
      , Html.ul []
          [ Html.li [] [ Html.text "e-mail, to security@[this domain]," ]
          , Html.li []
              [ Html.a
                  [ Attributes.href "https://github.com/bmillwood/flexiprocity/issues" ]
                  [ Html.text "GitHub issue tracker" ]
              , Html.text "."
              ]
          ]
      , Html.text """The latter is public, so obviously hazardous for critical
          or immediately exploitable issues."""
      ]
  , Html.p []
      [ Html.text """I'm running this website as a volunteer in my free time, so
          any resolution will be on a best-effort basis, but I hope that I can
          provide some sort of response within three days, and a full solution
          within two weeks."""
      ]
  ]

viewAccount : Model -> { deleteConfirmations : Set String } -> List (Html Msg)
viewAccount model { deleteConfirmations } =
  let
    confirmations =
      [ ( "your-stuff-gone"
        , "Your user data and ticks will be deleted,"
        )
      , ( "their-ticks-gone"
        , """Other people's ticks for you will be deleted, and can't be
          reinstated if you come back,"""
        )
      , ( "dishonorable"
        , """Deleting your account in order to avoid someone finding out that
          you matched them violates their trust and you shouldn't do it."""
        )
      ]
    allConfirmationIds = Set.fromList (List.map (\(i, _) -> i) confirmations)
    item (checkId, text) =
      Html.li [] [
        Html.label
          [ Attributes.for checkId ]
          [ Html.input
              [ Attributes.type_ "checkbox"
              , Attributes.id checkId
              , Attributes.checked (Set.member checkId deleteConfirmations)
              , Events.onCheck (\setTo ->
                  [Model.SetDeleteConfirm { id = checkId, setTo = setTo }]
                )
              ]
              []
          , Html.text text
          ]
      ]
  in
  [ viewLogin model
  , Html.h2 [] [ Html.text "Delete your account" ]
  , Html.p [] [
      Html.text """
        You can permanently and irrevocably delete your account on this page.
        Before you do so, you need to confirm acknowledgement of the following:
        """
    , Html.ul [] (List.map item confirmations)
    , Html.button
        [ Attributes.class "facebook-button"
        , Attributes.class "logout"
        , Attributes.disabled (deleteConfirmations /= allConfirmationIds)
        , Events.onClick
            [ Model.DeleteAccount
            , Model.StartLogout
            , Model.UrlReq { internal = True, url = "/" }
            ]
        ]
        [ Html.text "Delete my account" ]
    ]
  ]

view : Model -> Browser.Document Msg
view model =
  let
    header =
      [ Html.h1 [] [
          Html.a
            [ Attributes.href "/"
            , Attributes.style "text-decoration" "none"
            , Attributes.style "color" "inherit"
            ]
            [ Html.text "reciprocity" ]
        ]
      , let
          viewError { id, msg } =
            Html.li
              [ Attributes.class "error" ]
              [ Html.button
                  [ Events.onClick [Model.DismissError { id = id }]
                  , Attributes.style "margin-right" "0.2em"
                  ]
                  [ Html.text "✕" ]
              , Html.text msg
              ]
        in
        Html.ul
          [ Attributes.class "errors" ]
          (List.map viewError model.errors)
      ]
    linkIf condition href text =
      if condition
      then Html.a [ Attributes.href href ] [ Html.text text ]
      else Html.text text
    navBar =
      let
        atAccount =
          case model.page of
            Model.Account _ -> True
            _ -> False
      in
      [ [ linkIf (model.page /= Model.Root) "/" "home" ]
      , case model.apiLoggedIn of
          Model.LoggedIn _ ->
            [ Html.text " | "
            , linkIf (not atAccount) "/account" "account"
            ]
          _ -> []
      , [ Html.text " | "
        , linkIf (model.page /= Model.Privacy) "/privacy" "privacy"
        , Html.text " | "
        , linkIf (model.page /= Model.Security) "/security" "security"
        , Html.text " | "
        , Html.a
            [ Attributes.href "https://github.com/bmillwood/flexiprocity" ]
            [ Html.text "code" ]
        ]
      ] |> List.concat
  in
  { title =
      case model.page of
        Model.PageNotFound -> "Not found - reciprocity"
        Model.Root -> "reciprocity"
        Model.Columns -> "reciprocity"
        Model.Account _ -> "Account - reciprocity"
        Model.Privacy -> "Privacy - reciprocity"
        Model.Security -> "Security - reciprocity"
  , body =
      header
      ++ navBar
      ++ case model.page of
        Model.PageNotFound -> [ Html.p [] [ Html.text "Page not found" ] ]
        Model.Root -> viewRoot { customiseColumns = False } model
        Model.Columns -> viewRoot { customiseColumns = True } model
        Model.Account account -> viewAccount model account
        Model.Privacy -> viewPrivacy model
        Model.Security -> viewSecurity model
  }
