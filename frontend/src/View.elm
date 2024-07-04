module View exposing (view)

import Browser
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events
import Html.Keyed as Keyed
import Json.Decode
import Set exposing (Set)

import Model exposing (Model, Msg)
import Ports
import PrivacyPolicy
import SearchWords
import Sort

profileFbUser : Model -> Model.Profile -> Maybe Ports.FacebookUser
profileFbUser model profile = profile.facebookId |> Maybe.andThen (\i -> Dict.get i model.facebookUsers)

profileName : Model -> Model.Profile -> Maybe String
profileName model profile =
  case profileFbUser model profile of
    Nothing -> profile.name
    Just { name } -> Just name

viewUser : Model -> Model.Profile -> { isMe : Bool } -> Html Msg
viewUser model user { isMe } =
  let
    facebookUser = profileFbUser model user
    name =
      profileName model user
      |> Maybe.withDefault ("[id " ++ user.userId ++ "]")
    pictureUrl =
      case (user.picture, facebookUser |> Maybe.map .picture) of
        (Just url, _) -> Just url
        (_, Just { url }) -> Just url
        (Nothing, Nothing) -> Nothing
    picture =
      case pictureUrl of
        Just url ->
          Html.img
            [ Attributes.style "margin" "0.2em 0.4em"
            , Attributes.height 50
            , Attributes.width 50
            , Attributes.src url
            ]
            []
        Nothing ->
          Html.div
            [ Attributes.style "margin" "0.2em 0.4em"
            , Attributes.style "height" "50px"
            , Attributes.style "width" "50px"
            , Attributes.style "opacity" "0.5"
            ]
            [ Html.text "no pic" ]
  in
  Html.div
    [ Attributes.style "display" "flex"
    , Attributes.class "user"
    ]
    [ picture
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
                      , Attributes.style "width" "30em"
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

viewFacebookStatus : Model -> List (Html Msg)
viewFacebookStatus model =
  List.map (Html.p [] << List.singleton) (
      case (model.facebookEnabled, model.facebookLoggedIn, model.facebookFriends) of
        (True, _, Just friends) ->
          [ if List.isEmpty friends
            then Html.text "None of your Facebook friends use reciprocity 🙁"
            else
              [ String.fromInt (List.length friends)
              , " of your Facebook friends use reciprocity"
              ] |> String.concat |> Html.text
          ]
        (True, Model.LoggedIn _, Nothing) ->
          [ Html.text "Retrieving your Facebook friends..." ]
        (True, _, _) ->
          [ Html.span
              [ Attributes.class "error" ]
              [ Html.text "Log in with Facebook to see pictures, profile links etc." ]
          ]
        (False, _, _) ->
          []
  )

viewSearches : Model -> List (Html Msg)
viewSearches model =
  [ Html.table []
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
  ]

viewPeople : { customiseColumns : Bool } -> Model -> List (Html Msg)
viewPeople { customiseColumns } model =
  [ let
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
                      , Attributes.disabled (Dict.isEmpty model.youWouldChange || not (Dict.isEmpty model.pendingYouWould))
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
                        { withId = profile.userId
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
            -- The idea of the key is that it retriggers the animation when rows
            -- are reordered, which helps the user notice that it's happened
            -- This isn't really what keys are designed for, but... it works
            -- :shrug:
            ( profile.userId
            , Html.tr
                [ Attributes.style "animation-name" "fade-in"
                , Attributes.style "animation-duration" "1s"
                ]
                cols
            )
          profileCompare p1 p2 =
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
            , Sort.nullsLast compare (profileName model p1) (profileName model p2)
            ] |> Sort.lexicographic
          filterName profile =
            case profileName model profile of
              Nothing -> not (SearchWords.isActive model.nameSearch)
              Just n -> SearchWords.hasMatch model.nameSearch n
          filterBio profile =
            SearchWords.hasMatch model.bioSearch profile.bio
          filterAudience profile =
            case model.showMe of
              Just Model.Everyone ->
                profile.audience /= Model.Self
              Just Model.Friends ->
                profile.audience == Model.Friends
              _ ->
                False
          filterProfile profile =
            filterName profile && filterBio profile && filterAudience profile
          profiles =
            Dict.values model.profiles
            |> List.filter filterProfile
            |> List.sortWith profileCompare
        in
        Keyed.node "tbody" [] (
          if List.isEmpty profiles
          then
            [ ( "empty"
              , Html.td
                  [ Attributes.colspan (List.length colsById + 1)
                  , Attributes.style "padding" "1em"
                  , Attributes.style "font-style" "italic"
                  ]
                  [ Html.text "(No users visible)" ]
              )
            ]
          else List.map viewProfile profiles
        )
      ]
  ]

logoutButton : { loggingOut : Bool } -> Html Msg
logoutButton { loggingOut } =
  Html.button
    [ Attributes.class "auth"
    , Attributes.class "logout"
    , Attributes.disabled loggingOut
    , Events.onClick [Model.StartLogout]
    ]
    [ Html.text (if loggingOut then "Logging out..." else "Logout") ]

viewFacebookLogin : Model -> List (Html Msg)
viewFacebookLogin model =
  let
    button disabled text =
      Html.button
        [ Events.onClick [Model.StartFacebookLogin]
        , Attributes.disabled disabled
        , Attributes.class "auth"
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
    Model.Unknown ->
      [ button True "Checking Facebook login status" ]
    Model.NotLoggedIn ->
      [ button False "Login with Facebook" ]
    Model.LoggingIn ->
      [ button True "Logging in..." ]
    Model.LoggedIn { userId } ->
      [ Html.span
          [ Attributes.class "auth"
          , Attributes.class "facebook-logged-in"
          ]
          (viewLoggedIn userId)
      , logoutButton { loggingOut = False }
      ]
    Model.LoggingOut ->
      [ logoutButton { loggingOut = True } ]

viewGoogleLogin : Model -> List (Html Msg)
viewGoogleLogin model =
  let
    button disabled text =
      Html.button
        [ Events.onClick [Model.UrlReq { internal = False, url = "/auth/login/google/start" }]
        , Attributes.class "auth"
        , Attributes.disabled disabled
        ]
        [ Html.text text ]
  in
  case model.apiLoggedIn of
    Model.LoggedIn { googleEmail } ->
      case googleEmail of
        Nothing -> []
        Just email ->
          [ button True ("Logged in as " ++ email)
          , logoutButton { loggingOut = False }
          ]
    Model.NotLoggedIn ->
      [ button False "Log in with Google" ]
    Model.LoggingIn -> [ button True "Logging in..." ]
    Model.LoggingOut -> [ logoutButton { loggingOut = True } ]
    Model.Unknown -> [ button True "Checking login status..." ]

viewLogin : Model -> List (Html Msg)
viewLogin model =
  if not model.facebookEnabled && not model.googleEnabled
  then [ Html.p [] [ Html.text "No login methods available :(" ] ]
  else
    let
      pIfEnabled cond l = if cond then [ Html.p [] l ] else []
    in
    [ pIfEnabled model.facebookEnabled (viewFacebookLogin model)
    , pIfEnabled model.googleEnabled (viewGoogleLogin model)
    ] |> List.concat

viewAudienceControls : Model -> List (Html Msg)
viewAudienceControls model =
  let
    audienceRadio { name, currentWho, onCheck, who, label } =
      let
        thisId = name ++ "-" ++ Model.audienceToString who
        isChecked = currentWho == Just who
      in
      [ Html.input
        [ Attributes.type_ "radio"
        , Attributes.name name
        , Attributes.id thisId
        , Attributes.checked isChecked
        , Events.onCheck (\_ -> onCheck)
        ] []
      , Html.label
          [ Attributes.for thisId ]
          [ Html.text label ]
      ]
  in
  [ let
      radio who label =
        audienceRadio
          { name = "visibility"
          , currentWho = model.myVisibility
          , onCheck = [Model.SubmitVisibility who]
          , who = who
          , label = label
          }
    in
    [ [ Html.text "Show my profile to people I've ticked and:" ]
    , radio Model.Self "Nobody else"
    , if model.facebookEnabled
      then radio Model.Friends "Friends"
      else []
    , radio Model.Everyone "Everyone"
    ]
  , let
      amVisibleTo profile =
        let
          youWould = not (Set.isEmpty profile.youWouldIds)
          youAreFriends =
            case profile.friendsSince of
              Just _ -> True
              Nothing -> False
        in
        profile.audience /= Model.Self
        && case model.myVisibility of
          Nothing -> False
          Just Model.Self -> youWould
          Just Model.Friends -> youWould || youAreFriends
          Just Model.Everyone -> True
      visibleCount =
        List.filter amVisibleTo (Dict.values model.profiles)
        |> List.length
    in
    [ [ Html.text "Visible to "
      , Html.span
          (
            if visibleCount <= 0
            then [ Attributes.class "flag" ]
            else []
          )
          [ Html.text (String.fromInt visibleCount)
          , Html.text (
              if visibleCount == 1
              then " person"
              else " people"
            )
          ]
      , Html.text " (among those visible to you)"
      ]
    ]
  , let
      radio who label =
        audienceRadio
          { name = "search"
          , currentWho = model.showMe
          , onCheck = [Model.SubmitShowMe who]
          , who = who
          , label = label
          }
    in
    if model.facebookEnabled
    then
      [ [ Html.text "I want to see:" ]
      , radio Model.Friends "My friends"
      , radio Model.Everyone "Everyone"
      ]
    else
      []
  ]
  |> List.filter (not << List.isEmpty)
  |> List.map (Html.p [] << List.concat)

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
      , viewFacebookStatus model
      , viewSearches model
      , viewPeople { customiseColumns = customiseColumns } model
      , if customiseColumns then viewCustomiseColumns model { myUserId = userId } else []
      ] |> List.concat
    privacyPrompt reason =
      [ Html.div
          [ Attributes.style "padding-left" "1em"
          , Attributes.class "error"
          ]
          [ Html.p [] [Html.text reason]
          , Html.p [] [Html.text "Use the nav bar to head to the privacy page and take a look."]
          ]
      ]
  in
  [ viewLogin model
  , case model.apiLoggedIn of
      Model.LoggedIn { userId } ->
        case (model.latestPrivacyPolicy, model.myPrivacyPolicy) of
          (Nothing, _) -> normalPage { userId = userId }
          (Just _, Nothing) ->
            [ privacyPrompt "I think you're new here, so you'll need to agree to the privacy policy."
            , whatIsReciprocity
            ] |> List.concat
          (Just latestVersion, Just myVersion) ->
            if latestVersion == myVersion
            then normalPage { userId = userId }
            else
              privacyPrompt "Looks like the privacy policy has been updated since you agreed to it."
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
  , [ Html.h2 [] [ Html.text "Delete your account" ]
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
  ] |> List.concat

viewWhyNotFacebook : Model -> List (Html Msg)
viewWhyNotFacebook _ =
  [ Html.h2 []
      [ Html.text "What happened to Facebook integration?" ]
  , Html.p []
      [ Html.text """Original reciprocity integrated with Facebook to
            (optionally) show you just your friends. Indeed, this was arguably
            the core functionality of the app."""
      ]
  , Html.p []
      [ Html.text """I have written code to do the same thing here, but Facebook
            API access for real apps is only available to "verified business
            accounts". Initially I assumed I could just set up a company
            registration and verify it, but having looked into it more, they
            don't only verify that your company exists but also that you have
            been on Facebook for a while, you post on your page, you have
            engagement from Facebook users, that sort of thing. As such I can't
            run reciprocity with Facebook integration unless someone with a
            verified Facebook business account is willing to attach it to their
            page."""
      ]
  , Html.p []
      [ Html.text """Since I'd already written the code at this point, I just
            added another login method so that people could at least experiment
            with the design as it is now, and hopefully someone with the
            necessary Facebook relationship could see it and help me out. """
      , Html.text "Send an e-mail to contact@[this domain] if you can help?"
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
        , linkIf (model.page /= Model.WhyNotFacebook) "/why-not-facebook" "facebook?"
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
        Model.WhyNotFacebook -> "Facebook? - reciprocity"
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
        Model.WhyNotFacebook -> viewWhyNotFacebook model
  }
