module ElmerPress.View.Game (view) where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import ElmerPress.Game as Game exposing (scoreOf, winner)
import ElmerPress.Color as Color exposing (..)
import ElmerPress.View.Board as BoardView
import ElmerPress.View.Selection as SelectionView

view address wordQueriesAddress model =
  let turnView =
        div []
          [ text "Current turn: "
          , text (toString model.turn)
          ]
      scoreView =
        div []
          [ text "Red: "
          , text (toString (scoreOf Red model))
          , text " "
          , text "Blue: "
          , text (toString (scoreOf Blue model))
          ]
      gameOverView color =
        div []
          [text ("Game Over. " ++ (toString color) ++ " wins.")]
      hr = Html.hr [style [("clear", "both")]] []
  in
    div []
      [ turnView
      , hr
      , scoreView
      , hr
      , (case winner model of
          Nothing -> SelectionView.view address wordQueriesAddress model.selection
          Just color -> gameOverView color
        )
      , hr
      , BoardView.view address model.board
      ]
