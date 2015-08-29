module ElmerPress.Letter where

import ElmerPress.Color as Color exposing (..)

type alias Letter = { x: Int, y: Int, char: Char, color: Maybe Color, selected: Bool, locked: Bool }

hasColor color letter =
  case color of
    Just color -> letter.color == Just color
    Nothing    -> False

unselect letter =
  { letter | selected <- False }
