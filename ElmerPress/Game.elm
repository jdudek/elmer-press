module ElmerPress.Game
  ( Model
  , QueryStatus(..)
  , initModel
  , update
  , scoreOf
  , winner
  ) where

import Random

import ElmerPress.Action as Action exposing (..)
import ElmerPress.Board as Board exposing (Board)
import ElmerPress.Board.Random as RandomBoard
import ElmerPress.Color as Color exposing (..)
import ElmerPress.Letter as Letter exposing (Letter)
import ElmerPress.Selection as Selection exposing (Selection)

type QueryStatus = None | Progress | Invalid

type alias Model =
  { board : Board
  , selection : Selection
  , turn : Color
  , queryStatus : QueryStatus
  }

initModel : Random.Seed -> Model
initModel seed =
  { board = RandomBoard.init seed
  , selection = []
  , turn = Red
  , queryStatus = None
  }

update action model =
  let
    isUpdateAllowed =
      case action of
        Verified _ ->
          True

        _ ->
          model.queryStatus /= Progress

    update' model =
      case action of
        Select letter ->
          selectLetter letter model

        Unselect letter ->
          unselectLetter letter model

        Clear ->
          clearSelection model

        Query word ->
          { model | queryStatus <- Progress }

        Verified words ->
          if isCorrectWord (Selection.toWord model.selection) words then
            switchTurn model
          else
            { model | queryStatus <- Invalid }
  in
    if isGameOver model || not (isUpdateAllowed) then
      model
    else
      update' { model | queryStatus <- None }

-- private

isCorrectWord : String -> List (String, Bool) -> Bool
isCorrectWord word words =
  List.any (\(word', status) -> word == word' && status == True) words

scoreOf color model =
  Board.countLettersOfColor color model.board

winner model =
  let
    numberOfBlue =
      Board.countLettersOfColor Blue model.board

    numberOfRed =
      Board.countLettersOfColor Red model.board
  in
    if numberOfRed + numberOfBlue == Board.countLetters model.board then
      if numberOfBlue > numberOfRed then Just Blue else Just Red
    else Nothing

isGameOver model =
  case winner model of
    Just _  -> True
    Nothing -> False

selectLetter letter model =
  let
    newLetter =
      { letter | selected <- True }

    newBoard =
      Board.replaceLetter model.board letter newLetter

    newSelection =
      Selection.addLetter newLetter model.selection
  in
    { model | board <- newBoard, selection <- newSelection }

unselectLetter letter model =
  let
    newLetter =
      { letter | selected <- False }

    newBoard =
      Board.replaceLetter model.board letter newLetter

    newSelection =
      Selection.removeLetter letter model.selection
  in
    { model | board <- newBoard, selection <- newSelection }

clearSelection model =
  let
    newBoard =
      List.map Letter.unselect model.board
  in
    { model | board <- newBoard, selection <- [] }

switchTurn model =
  let
    markLetterColor letter =
      if Selection.member letter model.selection && not letter.locked
        then { letter | color <- Just model.turn, selected <- False }
        else { letter | selected <- False }

    markIfLetterLocked board letter =
      let
        neighbours =
          Board.neighboursOf letter board

        areAllNeighboursSameColor =
          List.all (Letter.hasColor letter.color) neighbours
      in
        { letter | locked <- areAllNeighboursSameColor }

    markLettersColors board =
      List.map (markLetterColor) board

    markIfLettersLocked board =
      List.map (markIfLetterLocked board) board

    newBoard =
      (markIfLettersLocked << markLettersColors) model.board
  in
    { model
      | board <- newBoard
      , selection <- []
      , turn <- Color.flip model.turn
    }
