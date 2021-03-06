module ElmerPress.Game
  ( Model
  , Action(..)
  , SubmissionStatus(..)
  , initModel
  , update
  , scoreOf
  , winner
  ) where

import Random
import Http
import Task exposing (Task, succeed, fail, andThen, onError)
import Effects exposing (Effects)
import Util.Effects exposing (..)

import ElmerPress.Board as Board exposing (Board)
import ElmerPress.Board.Random as RandomBoard
import ElmerPress.Color as Color exposing (..)
import ElmerPress.Letter as Letter exposing (Letter)
import ElmerPress.Selection as Selection exposing (Selection)

type SubmissionStatus = None | Progress | Invalid | AlreadyPlayed

-- model

type alias Model =
  { board : Board
  , selection : Selection
  , turn : Color
  , submissionStatus : SubmissionStatus
  , verifications : List (String, Bool)
  , playedWords : List String
  }

initModel : Random.Seed -> Model
initModel seed =
  { board = RandomBoard.init seed
  , selection = []
  , turn = Red
  , submissionStatus = None
  , verifications = []
  , playedWords = []
  }

-- update

type Action
  = Select Letter
  | Unselect Letter
  | Clear
  | Submit String
  | Verified String Bool

update : Action -> Model -> (Model, Effects Action)
update action model =
  let
    isUpdateAllowed =
      case action of
        Verified _ _ ->
          True

        _ ->
          model.submissionStatus /= Progress

    update' model =
      case action of
        Select letter ->
          noFx (selectLetter letter model)

        Unselect letter ->
          noFx (unselectLetter letter model)

        Clear ->
          noFx (clearSelection model)

        Submit word ->
          submitWord word model

        Verified word status ->
          handleVerification word status model
  in
    if isGameOver model || not (isUpdateAllowed) then
      noFx model
    else
      update' { model | submissionStatus <- None }

-- private

scoreOf : Color -> Model -> Int
scoreOf color model =
  let
    ofColor letter =
      letter.color == Just color

    ofOtherColor letter =
      letter.color == Just (Color.flip color)

    and f g x =
      f x && g x

    boardScore =
      List.length (List.filter (ofColor `and` (not << .selected)) model.board)

    selectionScoreOfCurrentTurn =
      List.length (List.filter (not << (ofOtherColor `and` .locked)) model.selection)

    selectionScoreOfOtherTurn =
      List.length (List.filter (ofColor `and` .locked) model.selection)
  in
    if color == model.turn then
      boardScore + selectionScoreOfCurrentTurn
    else
      boardScore + selectionScoreOfOtherTurn

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

submitWord word model =
  if List.member word model.playedWords  then
    noFx (setSubmissionStatus AlreadyPlayed model)
  else
    case findVerification word model.verifications of
      Just (word, status) ->
        update (Verified word status) model

      Nothing ->
        withFx (verifyWord word) (setSubmissionStatus Progress model)

handleVerification word status model =
  let
    model' =
      storeVerification (word, status) model

    selectedWord =
      Selection.toWord model.selection

    isCorrect =
      isCorrectWord selectedWord model'.verifications
  in
    noFx <|
      if isCorrect then
        (switchTurn << storePlayedWord selectedWord) model'
      else
        setSubmissionStatus Invalid model

setSubmissionStatus status model =
  { model | submissionStatus <- status }

storeVerification verification model =
  { model | verifications <- verification :: model.verifications }

storePlayedWord word model =
  { model | playedWords <- word :: model.playedWords }

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

verifyWord word =
  let
    url =
      "http://letterpress-words-api.herokuapp.com/" ++ Http.uriEncode(word)

    succeedIf200 _ =
      succeed (Verified word True)

    succeedIf404 err =
      case err of
        Http.BadResponse 404 _ -> succeed (Verified word False)
        _ -> succeed (Submit word)

    request =
      (Http.getString url `andThen` succeedIf200) `onError` succeedIf404
  in
    Effects.task request

isCorrectWord : String -> List (String, Bool) -> Bool
isCorrectWord word words =
  List.any (\(word', status) -> word == word' && status == True) words

findVerification : String -> List (String, Bool) -> Maybe (String, Bool)
findVerification word words =
  List.head (List.filter ((==) word << fst) words)
