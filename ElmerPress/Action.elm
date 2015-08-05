module ElmerPress.Action where

import ElmerPress.Letter exposing (Letter)

type Action = Select Letter | Unselect Letter | Query String | Verified (List (String, Bool))
