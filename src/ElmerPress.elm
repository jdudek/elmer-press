module ElmerPress where

import Effects exposing (Effects)
import Html exposing (Html, text, div, h1, button)
import Html.Events exposing (onClick)
import Random
import StartApp
import Task exposing (Task)
import Time
import Util.Effects exposing (..)

import ElmerPress.Game as Game
import ElmerPress.Color exposing (Color(..))
import ElmerPress.View.Game as GameView

type alias Id = Int

type alias Model =
  { games : List (Id, Game.Model)
  , current : Maybe Id
  , seed : Random.Seed
  }

type Action
  = Seeded Random.Seed
  | AddGame
  | OpenGame Id
  | BackToList
  | GameAction Id Game.Action

main =
  app.html

port tasks : Signal (Task Effects.Never ())
port tasks =
  app.tasks

app =
  let
    model = { games = [], current = Nothing, seed = (Random.initialSeed 0) }
  in
    StartApp.start
      { init = (model, Effects.none)
      , view = view
      , update = update
      , inputs = [Signal.map Seeded seeds]
      }

update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    AddGame ->
      let
        id =
          List.length model.games

        game =
          Game.initModel model.seed
      in
        noFx { model | games <- (id, game) :: model.games, current <- Just id }

    OpenGame id ->
      noFx { model | current <- Just id }

    BackToList ->
      noFx { model | current <- Nothing }

    GameAction id action ->
      let
        update' (id', game) =
          if id' == id then
            let
              (newGame, fx) = (Game.update action game)
            in
              ((id, newGame), Effects.map (GameAction id) fx)
          else
            ((id', game), Effects.none)

        (games, fxs) =
          List.unzip (List.map update' model.games)

      in
        ({ model | games <- games }, Effects.batch fxs)

    Seeded seed ->
      noFx { model | seed <- seed }

findGame : Id -> Model -> Maybe (Id, Game.Model)
findGame id model =
  List.head (List.filter ((==) id << fst) model.games)

getCurrentGame : Model -> Maybe (Id, Game.Model)
getCurrentGame model =
  case model.current of
    Nothing -> Nothing
    Just id ->
      findGame id model

view : Signal.Address Action -> Model -> Html
view address model =
  let
    mainView : Html
    mainView =
      case getCurrentGame model of
        Nothing ->
          listView address model

        Just (id, game) ->
          GameView.view
            (Signal.forwardTo address (GameAction id))
            game
  in
    div []
      [ Html.h1 [onClick address BackToList] [text "ElmerPress"]
      , mainView
      ]

listView : Signal.Address Action -> Model -> Html
listView address model =
  let
    buttonView =
      div []
        [button [onClick address AddGame] [text "Start new game"]]
  in
    div []
      ((List.map (gameSummaryView address) model.games) ++ [buttonView])

gameSummaryView : Signal.Address Action -> (Id, Game.Model) -> Html
gameSummaryView address (id, game) =
  div []
    [ text ("#" ++ (toString id) ++ " ")
    , text "Red: "
    , text (toString (Game.scoreOf Red game))
    , text " "
    , text "Blue: "
    , text (toString (Game.scoreOf Blue game))
    , text " "
    , button [onClick address (OpenGame id)] [text "Play!"]
    ]

seeds : Signal Random.Seed
seeds =
  let
    timeSignal =
      Time.every 100

    intSignal =
      Signal.map (round << ((*) 1000)) timeSignal
  in
    Signal.map Random.initialSeed intSignal
