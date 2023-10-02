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

matches : Model -> String -> Bool
matches { terms, composing } input =
  let
    contains search =
      String.contains (String.trim search)
        (if search == String.toLower search
          then String.toLower input
          else input
        )
  in
  if List.isEmpty terms
  then contains composing
  else
    List.all contains terms
    && not (String.isEmpty composing) && contains composing

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
        [x] -> ({ model | composing = input }, Cmd.none)
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
