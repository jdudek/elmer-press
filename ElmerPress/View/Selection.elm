module ElmerPress.View.Selection (view) where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import ElmerPress.Action as Action exposing (..)
import ElmerPress.Game exposing (QueryStatus(..))
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

    messageViewItems =
      if model.queryStatus == Invalid then
        [ text " "
        , i [] [text (Selection.toWord model.selection)]
        , text " is not a valid word!"
        ]
      else
        []
  in
    div []
      (letterViews ++ buttonViews ++ messageViewItems)

selectionLetterStyle =
  [ ("float", "left")
  ]

selectedLetterView address letter =
  Letter.view address letter selectionLetterStyle (Unselect letter)

submitButton address model =
  let
    caption =
      if isSubmitting model then "Submitting…" else "Submit"
  in
    button
      [ onClick address (Query (Selection.toWord model.selection))
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
  model.queryStatus == Progress

isButtonDisabled model =
  List.isEmpty model.selection || isSubmitting model

