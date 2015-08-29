module ElmerPress.Color where

type Color = Red | Blue

flip color =
  case color of
    Red  -> Blue
    Blue -> Red
