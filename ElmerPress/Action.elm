module ElmerPress.Action where

import ElmerPress.Letter exposing (Letter)

type Action
  = Select Letter
  | Unselect Letter
  | Clear
  | Query String
  | Verified (List (String, Bool))
