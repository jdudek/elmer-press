module ElmerPress.Verification (fromActions) where

import Http
import Task exposing (Task, succeed, fail, andThen, onError)

import ElmerPress.Action as Action exposing (..)

type alias Query = Maybe String

fromActions inputActions =
  let
    verification =
      init (queriesFromActions inputActions)

    verificationActions =
      Signal.map (\list -> Just (Verified list)) verification.combinedResults
  in
    { actions = verificationActions
    , tasks = verification.tasks
    }

-- private

init queries =
  let
    results =
      Signal.mailbox ("", False)

    combinedResults =
      Signal.foldp (::) [] results.signal

    tasks =
      Signal.map (taskFromQuery results.address) queries
  in
    { queries = queries
    , tasks = tasks
    , results = results.signal
    , combinedResults = combinedResults
    }

requestWord word address =
  let
    url =
      "http://letterpress-words-api.herokuapp.com/" ++ Http.uriEncode(word)

    succeedIf200 _ =
      succeed (word, True)

    succeedIf404 err =
      case err of
        Http.BadResponse 404 _ -> succeed (word, False)
        _ -> fail err

    request =
      (Http.getString url `andThen` succeedIf200) `onError` succeedIf404

    sendResult result =
      Signal.send address result
  in
    request `andThen` sendResult

taskFromQuery address query =
  case query of
    Nothing ->
      succeed ()

    Just word ->
      requestWord word address

queriesFromActions : Signal (Maybe Action) -> Signal Query
queriesFromActions actions =
  let
    toQuery action =
      case action of
        Just (Query word) -> Just word
        _ -> Nothing
  in
    Signal.map toQuery actions
