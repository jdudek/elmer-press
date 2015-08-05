module ElmerPress where

import Debug
import Random
import Time
import Signal.Extra
import Http
import Task exposing (Task, succeed, fail, andThen, onError)

import ElmerPress.Game as Game
import ElmerPress.View.Game as GameView
import ElmerPress.Action as Action exposing (..)

main =
  let
    update = Game.update

    view = GameView.view actionsAddress

    inputs = Signal.map2 (,) actions.signal seeds

    models =
      let
        updateFromInput ((Just action), seed) model = update action model
      in
        Signal.Extra.foldp' updateFromInput (Game.initModel << snd) inputs
  in
    Signal.map view models

actions =
  Signal.mailbox Nothing

actionsAddress =
  Signal.forwardTo actions.address Just

seeds : Signal Random.Seed
seeds =
  let
    timeSignal =
      Signal.map fst (Time.timestamp (Signal.constant ()))
  in
    Signal.map (Random.initialSeed << round << ((*) 1000)) timeSignal

wordQueries =
  let
    isQuery action =
      case action of
        Just (Query _) -> True
        _ -> False

    toJustWord action =
      case action of
        Just (Query word) -> Just word
        Nothing -> Nothing
  in
    Signal.map toJustWord (Signal.filter isQuery Nothing actions.signal)

wordRequestResults : Signal.Mailbox (String, Bool)
wordRequestResults =
  Signal.mailbox ("", False)

knownWords : Signal (List (String, Bool))
knownWords =
  Signal.foldp (::) [] wordRequestResults.signal

requestWord word =
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
      Signal.send wordRequestResults.address result
  in
    request `andThen` sendResult

port wordRequestTasks : Signal (Task Http.Error ())
port wordRequestTasks =
  let
    doRequest query =
      case query of
        Nothing ->
          succeed ()

        Just word ->
          requestWord word
  in
    Signal.map doRequest wordQueries

port actionsFromKnownWords : Signal (Task x ())
port actionsFromKnownWords =
  Signal.map (\list -> Signal.send actionsAddress (Verified list)) knownWords
