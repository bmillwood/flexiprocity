module View exposing (view)

import Browser
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events
import Set exposing (Set)

import Model exposing (Model, Msg)
import SearchWords

viewUser : Model -> Model.Profile -> { isMe : Bool } -> Html Msg
viewUser model user { isMe } =
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
            , Attributes.style "margin" "0.2em 0.4em"
            ]
            []
    , Html.div
        []
        [ Html.div
            [ Attributes.style "font-weight" "bold" ]
            [ case facebookUser |> Maybe.andThen .link of
                Just link ->
                  Html.a
                    [ Attributes.href link ]
                    [ Html.text name ]
                Nothing -> Html.text name
            ]
        , Html.div
            [ Attributes.style "margin" "0.1em 0.5em" ]
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
              [ Html.text user.bio ]
          )
        ]
    ]

viewPeople : Model -> List (Html Msg)
viewPeople model =
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
      wouldsById = Dict.toList model.wouldsById
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
              wouldCol (_, wName) =
                Html.th
                  [ Attributes.style "width" "10%" ]
                  [ Html.text wName ]
              cols =
                Html.th
                  [ Attributes.style "text-align" "left"
                  , Attributes.style "padding-left" "1em"
                  ]
                  [ Html.text "People" ]
                :: List.map wouldCol wouldsById
            in
            Html.tr [] cols
          , Html.tr
              []
              [ Html.th [] []
              , Html.th
                  [ Attributes.colspan (List.length wouldsById) ]
                  [ Html.button
                      [ Events.onClick [Model.SubmitWouldChanges]
                      , Attributes.disabled (Dict.isEmpty model.wouldChange)
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
                |> List.filterMap (\i -> Dict.get i model.wouldsById)
                |> Set.fromList
              youWouldNames = toWouldNames profile.youWouldIds
              matchedNames = toWouldNames profile.matchedWouldIds
              wouldCol (wId, wName) =
                let
                  isMatched = Set.member wName matchedNames
                  isYouWould = Set.member wName youWouldNames
                  isModified =
                    Dict.get profile.userId model.wouldChange
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
                    [ Model.WouldChange
                        { userId = profile.userId
                        , wouldId = wId
                        , changeTo = newChecked
                        }
                    ]
                  isChecked =
                    case
                      Dict.get profile.userId model.wouldChange
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
                :: List.map wouldCol wouldsById
            in
            Html.tr [] cols
          profileAntiPriority profile =
            ( negate (Set.size profile.matchedWouldIds)
            , negate (Set.size profile.youWouldIds)
            , case Dict.get profile.facebookId model.facebookUsers of
                Nothing -> ""
                Just { name } -> name
            )
          sortProfiles = List.sortBy profileAntiPriority
          filterName profile =
            Dict.get profile.facebookId model.facebookUsers
            |> Maybe.map (SearchWords.matches model.nameSearch << .name)
            |> Maybe.withDefault False
          filterBio profile = SearchWords.matches model.bioSearch profile.bio
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
        Html.tbody [] (List.map viewProfile profiles)
      ]
  ]

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

viewRoot : Model -> List (Html Msg)
viewRoot model =
  [ [ viewLogin model ]
  , case model.apiLoggedIn of
      Model.LoggedIn { userId } ->
        viewAudienceControls model
        ++ case Dict.get userId model.profiles of
          Just u -> [viewUser model u { isMe = True }]
          Nothing -> []
      _ -> []
  , case model.facebookFriends of
      Nothing -> []
      Just friends ->
        [ Html.p []
            [ [ if List.isEmpty friends
                then "None"
                else String.fromInt (List.length friends)
              , " of your Facebook friends use flexiprocity"
              , if List.isEmpty friends
                then " 🙁"
                else ""
              ] |> String.concat |> Html.text
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
        ] ++ viewPeople model
  ] |> List.concat

viewPrivacy : Model -> List (Html Msg)
viewPrivacy _ =
  [ Html.h2 []
      [ Html.text "Privacy policy" ]
  , Html.p []
      [ Html.text """Below are detailed the kinds of data collected for the app,
          and the purposes for which it is collected. The purposes are intended
          to be exhaustive: no data is collected or can be used for any other
          purpose without your express prior consent."""
      ]
  , Html.p []
      -- https://developers.facebook.com/terms/#privacypolicy
      [ Html.text "The app uses data from Facebook as follows:"
      , [ """Your name and profile picture, to show to other users of the app
            according to your visibility settings,"""
        , """The list of your friends who use the app, to determine which
            profiles to show to you, and who can view your profile."""
        , """A link to your Facebook profile, so users can see more of your
            content on Facebook (though the app has no access to that content
            directly)."""
        ]
        |> List.map (Html.li [] << List.singleton << Html.text)
        |> Html.ul []
      , Html.text "The app uses data provided directly by you:"
      , [ "Your bio, to display to other users according to your visibility settings,"
        , """Your preferences and settings, in order to customise your experience
            of the app."""
        ]
        |> List.map (Html.li [] << List.singleton << Html.text)
        |> Html.ul []
      , Html.text """Moreover, certain technical information about your
          connection to the app is automatically gathered:"""
      , [ """User IP addresses, browser versions, and site resource access patterns
            are stored temporarily for the purpose of monitoring and debugging
            the app or the system. They are not accessed automatically as part
            of app functionality or used for any other purpose."""
        ]
        |> List.map (Html.li [] << List.singleton << Html.text)
        |> Html.ul []
      ]
  , Html.p []
      [ Html.text "You can delete all the data held about you on your "
      , Html.a
          [ Attributes.href "/account" ]
          [ Html.text "account" ]
      , Html.text " page."
      ]
  , Html.p [] [
      Html.text """In general, the app keeps this data even when you don't
        strictly use the functionality that needs it (e.g. your friend list when
        your visibility is set to everyone). This is purely for simplicity of
        implementation and reflects no intention to use the data for any other
        purpose."""
    ]
  , Html.p [] [
      Html.text """This data is stored on and transmitted to e.g. hosting and
        service providers as necessary for the operation of the app. Any third
        party who handles this data will be subject to all the restrictions and
        obligations of this policy."""
    ]
  , Html.p [] [
      Html.text """If this policy is changed for any reaosn, you will continue
        to be subject to the terms of the old policy until you explicitly agree
        to the new one."""
    ]
  ]

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
          you matched them violates their trust and you shouldn't do it. One day
          I'll implement some technical countermeasures for this, but they don't
          exist yet."""
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
            [ Html.text "flexiprocity" ]
        ]
      , let
          viewError { id, msg } =
            Html.li []
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
        Model.PageNotFound -> "Not found - flexiprocity"
        Model.Root -> "flexiprocity"
        Model.Account _ -> "Account - flexiprocity"
        Model.Privacy -> "Privacy - flexiprocity"
        Model.Security -> "Security - flexiprocity"
  , body =
      header
      ++ navBar
      ++ case model.page of
        Model.PageNotFound -> [ Html.p [] [ Html.text "Page not found" ] ]
        Model.Root -> viewRoot model
        Model.Account account -> viewAccount model account
        Model.Privacy -> viewPrivacy model
        Model.Security -> viewSecurity model
  }
