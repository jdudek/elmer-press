module ElmerPress.Action where

import ElmerPress.Letter exposing (Letter)

type Action
  = Select Letter
  | Unselect Letter
  | Clear
  | Submit String
  | Verified String Bool
