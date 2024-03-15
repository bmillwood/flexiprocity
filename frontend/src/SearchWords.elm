module SearchWords exposing (..)

import Browser.Dom
import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events
import Json.Decode
import Task

type alias Model =
  { terms : List String
  , composing : String
  , htmlInputId : String
  }

span : (a -> Bool) -> List a -> (List a, List a)
span p xs =
  case xs of
    [] -> ([], [])
    y :: ys ->
      if p y
      then
        case span p ys of
          (before, after) -> (y :: before, after)
      else ([], xs)

smartCase : (String -> String -> a) -> String -> String -> a
smartCase f search input =
  f
    (String.trim search)
    (if String.toLower search == search
      then String.toLower input
      else input
    )

highlightMatches : Model -> String -> List (Html msg)
highlightMatches { terms, composing } input =
  let
    toHtml from level changes =
      let
        percentage = String.fromInt (min 100 (level * 20)) ++ "%"
        atLevel start end =
          let
            text = Html.text (String.slice start end input)
          in
          if level <= 0
          then text
          else
            Html.span
              [ Attributes.style
                  "background-color"
                  ("hsla(60, 100%, 50%, " ++ percentage ++ ")")
              ]
              [ text ]
      in
      case changes of
        [] -> [atLevel from (String.length input)]
        (at, by) :: rest ->
          atLevel from at
          :: toHtml at (level + by) rest
  in
  List.concatMap
    (\term ->
      smartCase String.indices term input
      |> List.concatMap (\i -> [(i, 1), (i + String.length term, -1)])
    )
    (composing :: terms)
  |> List.sort
  |> toHtml 0 0

hasMatch : Model -> String -> Bool
hasMatch { terms, composing } input =
  List.all (\t -> smartCase String.contains t input) (composing :: terms)

type InMsg
  = Delete String
  | Compose String

type alias OutMsg = InMsg

init : { htmlInputId : String } -> Model
init { htmlInputId } = { terms = [], composing = "", htmlInputId = htmlInputId }

update : Model -> InMsg -> (Model, Cmd (List OutMsg))
update model msg =
  let
    focusInput =
      Browser.Dom.focus model.htmlInputId
      |> Task.attempt (\_ -> [])
  in
  case msg of
    Delete term ->
      ( { model | terms = List.filter (\t -> t /= term) model.terms }
      , focusInput
      )
    Compose input ->
      case List.reverse (String.words input) of
        [] -> ({ model | composing = "" }, Cmd.none)
        [_] -> ({ model | composing = input }, Cmd.none)
        newComposing :: newTerms ->
          let
            isOld t = List.member t model.terms
          in
          ( { model
            | terms = List.filter (not << isOld) newTerms ++ model.terms
            , composing = newComposing
            }
          , focusInput
          )

view : Model -> Html (List OutMsg)
view model =
  let
    viewTerm term =
      Html.span
        [ Attributes.class "term"
        ]
        [ Html.text term
        , Html.button
            [ Events.onClick [Delete term]
            ]
            [ Html.text "✕" ]
        ]
    onKeydown i =
      if i == 8 && String.isEmpty model.composing
      then case model.terms of
        [] -> []
        t :: _ ->
          [ Delete t
          , Compose t
          ]
      else []
    composeInput =
      [ Html.input
          [ Attributes.type_ "text"
          , Attributes.placeholder "words"
          , Events.onInput (List.singleton << Compose)
          , Events.on "keydown" (Json.Decode.map onKeydown Events.keyCode)
          , Attributes.id model.htmlInputId
          , Attributes.value model.composing
          ]
          []
      ]
  in
  Html.span
    [ Attributes.class "searchwords" ]
    (List.map viewTerm (List.reverse model.terms) ++ composeInput)
