module View exposing (view)

import Browser
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events
import Set exposing (Set)

import Model exposing (Model, Msg)

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
              toNames ids =
                List.filterMap (\i -> Dict.get i.wouldId model.wouldsById) ids
                |> Set.fromList
              youWouldNames = toNames profile.youWould
              matchedNames = toNames profile.matchedWoulds
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
                in
                Html.td
                  styles
                  [ let
                      isChecked =
                        case
                          Dict.get profile.userId model.wouldChange
                          |> Maybe.withDefault Dict.empty
                          |> Dict.get wId
                        of
                          Nothing -> Set.member wName youWouldNames
                          Just b -> b
                    in
                    Html.input
                      [ Attributes.type_ "checkbox"
                      , Attributes.disabled isMatched
                      , Attributes.checked isChecked
                      , Events.onCheck (\newChecked ->
                          [ Model.WouldChange
                              { userId = profile.userId
                              , wouldId = wId
                              , changeTo = newChecked
                              }
                          ]
                        )
                      ]
                      []
                  ]
              cols =
                Html.td [] [viewUser model profile { isMe = False }]
                :: List.map wouldCol wouldsById
            in
            Html.tr [] cols
          profiles =
            case model.showMe of
              Model.Everyone ->
                Dict.values model.profiles
                |> List.filter (\profile -> profile.audience /= Model.Self)
              Model.Friends ->
                Dict.values model.profiles
                |> List.filter (\profile -> profile.audience == Model.Friends)
              Model.Self -> []
        in
        Html.tbody [] (List.map viewProfile profiles)
      ]
  ]

view : Model -> Browser.Document Msg
view model =
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
  { title = "flexiprocity"
  , body =
      [ [ Html.h1 [] [ Html.text "flexiprocity" ]
        , let
            viewError err = Html.li [] [Html.text err]
          in
          Html.ul [] (List.map viewError model.errors)
        ]
        , let
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
          case model.facebookLoggedIn of
            Model.Unknown ->
              [Html.p [] [button True "Checking Facebook login status"]]
            Model.NotLoggedIn ->
              [Html.p [] [button False "Login with Facebook"]]
            Model.LoggingIn ->
              [Html.p [] [button True "Logging in..."]]
            Model.LoggedIn { userId } ->
              [ Html.p []
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
              , let
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
            Model.LoggingOut ->
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
          Model.LoggedIn { userId } ->
            case Dict.get userId model.profiles of
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
            ] ++ viewPeople model
      ] |> List.concat
  }

