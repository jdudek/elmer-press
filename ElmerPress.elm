module ElmerPress where

import Effects
import Html exposing (Html, text)
import Random
import StartApp
import Task exposing (Task)
import Time

import ElmerPress.Game as Game
import ElmerPress.View.Game as GameView
import ElmerPress.Action as GameAction

app =
  let
    model : Maybe Game.Model
    model = Nothing
  in
    StartApp.start
      { init = (model, Effects.none)
      , view = view
      , update = update
      , inputs = [Signal.map Seeded seeds]
      }

main =
  app.html

port tasks : Signal (Task Effects.Never ())
port tasks =
  app.tasks

type alias Model = Maybe Game.Model

type Action = Seeded Random.Seed | GameAction GameAction.Action

update action model =
  case (action, model) of
    (Seeded seed, Nothing) ->
      (Just (Game.initModel seed), Effects.none)

    (GameAction action, Just model) ->
      let
        (model', effects) = Game.update action model
      in
        (Just model', Effects.map GameAction effects)

    (_, Just model) ->
      (Just model, Effects.none)

    (_, Nothing) ->
      (Nothing, Effects.none)

view address model =
  case model of
    Nothing ->
      text ""

    Just model ->
      GameView.view (Signal.forwardTo address GameAction) model

seeds : Signal Random.Seed
seeds =
  let
    timeSignal =
      Time.every 100

    intSignal =
      Signal.map (round << ((*) 1000)) timeSignal
  in
    Signal.map Random.initialSeed intSignal
