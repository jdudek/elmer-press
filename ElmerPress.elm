module ElmerPress where

import Http
import Html exposing (Html)
import Random
import Signal.Extra
import Task exposing (Task)
import Time

import ElmerPress.Game as Game
import ElmerPress.View.Game as GameView
import ElmerPress.Action as Action exposing (..)
import ElmerPress.Verification as Verification

main : Signal Html
main =
  Signal.map view models

models : Signal Game.Model
models =
  let
    updateFromInput ((Just action), seed) model = Game.update action model
  in
    Signal.Extra.foldp' updateFromInput (Game.initModel << snd) inputs

inputs : Signal (Maybe Action, Random.Seed)
inputs =
  let
    allActions =
      Signal.merge actions.signal verification.actions
  in
    Signal.map2 (,) allActions seeds

view =
  GameView.view actions.address

actions : { signal : Signal (Maybe Action), address : Signal.Address Action }
actions =
  let
    mailbox = Signal.mailbox Nothing
    address = Signal.forwardTo mailbox.address Just
  in
    { mailbox | address <- address }

seeds : Signal Random.Seed
seeds =
  let
    timeSignal =
      Signal.map fst (Time.timestamp (Signal.constant ()))
  in
    Signal.map (Random.initialSeed << round << ((*) 1000)) timeSignal

verification :
  { actions : Signal (Maybe Action), tasks : Signal (Task Http.Error ()) }
verification =
  Verification.fromActions actions.signal

port runTasks : Signal (Task Http.Error ())
port runTasks = verification.tasks
