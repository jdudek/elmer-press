module ElmerPress.View.Selection (view) where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import ElmerPress.Action as Action exposing (..)
import ElmerPress.Selection as Selection exposing (..)
import ElmerPress.View.Letter as Letter

view address selection =
  let
    letterViews =
      List.map (selectedLetterView address) selection

    buttonViews =
      [ submitButton address selection
      , clearButton address selection
      ]
  in
    div []
      (letterViews ++ buttonViews)

selectionLetterStyle =
  [ ("float", "left")
  ]

selectedLetterView address letter =
  Letter.view address letter selectionLetterStyle (Unselect letter)

submitButton address selection =
  button
    [ onClick address (Query (Selection.toWord selection))
    , disabled (List.isEmpty selection)
    ]
    [text "Submit"]

clearButton address selection =
  button
    [ onClick address Clear
    , disabled (List.isEmpty selection)
    ]
    [text "Clear"]
