module ElmerPress.View.Letter (view, letterSize) where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import String

import ElmerPress.Color as Color exposing (..)

view address letter additionalStyle action =
  div
    [ style ((letterStyle letter) ++ additionalStyle)
    , onClick address action
    ]
    [text (String.fromChar letter.char)]

letterSize = 24

palette =
  { gray      = "#E9E8E5"
  , altGray   = "#E6E5E2"
  , lightRed  = "#F7998D"
  , lightBlue = "#78C8F5"
  , red       = "#FF432E"
  , blue      = "#00A2FF"
  }

letterStyle letter =
  let background =
    case letter.color of
      Just Blue ->
        (if letter.locked then .blue else .lightBlue) palette

      Just Red ->
        (if letter.locked then .red  else .lightRed)  palette

      Nothing ->
        palette.gray
  in
    [ ("display", "block")
    , ("width",  (toString letterSize) ++ "px")
    , ("height", (toString letterSize) ++ "px")
    , ("line-height", (toString letterSize) ++ "px")
    , ("text-align", "center")
    , ("text-transform", "uppercase")
    , ("background", background)
    ]
