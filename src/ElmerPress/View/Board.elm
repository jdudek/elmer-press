module ElmerPress.View.Board (view) where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import ElmerPress.Game exposing (Action(..))
import ElmerPress.View.Letter as Letter exposing (letterSize)

view address board =
  div [style boardStyle]
    (List.map (boardLetterView address) board)

boardStyle =
  [ ("width", "120px")
  , ("position", "relative")
  ]

boardLetterView address letter =
  Letter.view address letter (boardLetterStyle letter) (Select letter)

boardLetterStyle letter =
  [ ("visibility", if letter.selected then "hidden" else "visible")
  , ("position", "absolute")
  , ("top",  (toString (letterSize * letter.y)) ++ "px")
  , ("left", (toString (letterSize * letter.x)) ++ "px")
  ]
