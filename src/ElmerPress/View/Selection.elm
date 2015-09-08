module ElmerPress.View.Selection (view) where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import ElmerPress.Game exposing (SubmissionStatus(..), Action(..))
import ElmerPress.Selection as Selection exposing (..)
import ElmerPress.View.Letter as Letter

view address model =
  let
    selection = model.selection

    letterViews =
      List.map (selectedLetterView address) selection

    buttonViews =
      [ submitButton address model
      , clearButton address model
      ]
  in
    div []
      (letterViews ++ buttonViews ++ statusViewItems model)

selectionLetterStyle =
  [ ("float", "left")
  ]

selectedLetterView address letter =
  Letter.view address letter selectionLetterStyle (Unselect letter)

statusViewItems model =
  case model.submissionStatus of
    AlreadyPlayed ->
      [ text " "
      , i [] [text (Selection.toWord model.selection)]
      , text " has already been played!"
      ]

    Invalid ->
      [ text " "
      , i [] [text (Selection.toWord model.selection)]
      , text " is not a valid word!"
      ]

    _ ->
      []

submitButton address model =
  let
    caption =
      if isSubmitting model then "Submitting…" else "Submit"
  in
    button
      [ onClick address (Submit (Selection.toWord model.selection))
      , disabled (isButtonDisabled model)
      ]
      [text caption]

clearButton address model =
  button
    [ onClick address Clear
    , disabled (isButtonDisabled model)
    ]
    [text "Clear"]

isSubmitting model =
  model.submissionStatus == Progress

isButtonDisabled model =
  List.isEmpty model.selection || isSubmitting model

