module ElmerPress.View.Selection (view) where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import ElmerPress.Action as Action exposing (..)
import ElmerPress.Selection as Selection exposing (..)
import ElmerPress.View.Letter as Letter

view address wordQueriesAddress selection =
  div []
    ((List.map (selectedLetterView address) selection) ++ [submitButton wordQueriesAddress selection])

selectionLetterStyle =
  [ ("float", "left")
  ]

selectedLetterView address letter =
  Letter.view address letter selectionLetterStyle (Unselect letter)

submitButton wordQueriesAddress selection =
  button
    [ onClick wordQueriesAddress (Selection.toWord selection)
    , disabled (List.isEmpty selection)
    ]
    [text "Submit"]
