module ElmerPress where

import Html exposing (div, span, text, button)
import Html.Attributes exposing (style, disabled)
import Html.Events exposing (onClick)
import String
import Debug
import Random
import Time
import Signal.Extra
import Http
import Task exposing (Task, succeed, fail, andThen, onError)

import ElmerPress.Board as Board exposing (Board)
import ElmerPress.Color as Color exposing (..)
import ElmerPress.Letter as Letter exposing (..)
import ElmerPress.Selection as Selection exposing (Selection)

main =
  let
    inputs = Signal.map2 (,) actions.signal seeds

    models =
      let
        updateFromInput ((Just action), seed) model = update action model
      in
        Signal.Extra.foldp' updateFromInput (newModel << snd) inputs
  in
    Signal.map (view actionsAddress) models

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

type alias Model = { board : Board, selection: Selection, turn: Color }

newModel : Random.Seed -> Model
newModel seed =
  { board = Board.initRandom seed, selection = [], turn = Red }

wordQueries =
  let
    mailbox =
      Signal.mailbox Nothing
    address =
      Signal.forwardTo mailbox.address Just
  in
    { mailbox | address <- address }

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
    Signal.map doRequest wordQueries.signal

port actionsFromKnownWords : Signal (Task x ())
port actionsFromKnownWords =
  Signal.map (\list -> Signal.send actionsAddress (Verified list)) knownWords

-- update

type Action = Select Letter | Unselect Letter | Verified (List (String, Bool))

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

update action model =
  if isGameOver model
  then model
  else
    case action of
      Select letter ->
        selectLetter letter model

      Unselect letter ->
        unselectLetter letter model

      Verified words ->
        if isCorrectWord (Selection.toWord model.selection) words
        then switchTurn model
        else model

-- view

view address model =
  let turnView =
        div []
          [ text "Current turn: "
          , text (toString model.turn)
          ]
      scoreView =
        div []
          [ text "Red: "
          , text (toString (scoreOf Red model))
          , text " "
          , text "Blue: "
          , text (toString (scoreOf Blue model))
          ]
      submitButton =
        button
          [ onClick wordQueries.address (Selection.toWord model.selection)
          , disabled (List.isEmpty model.selection)
          ]
          [text "Submit"]
      selectionView =
        div []
          ((List.map (selectedLetterView address) model.selection) ++ [submitButton])
      boardView =
        div [style boardStyle]
          (List.map (boardLetterView address) model.board)
      gameOverView color =
        div []
          [text ("Game Over. " ++ (toString color) ++ " wins.")]
      hr = Html.hr [style [("clear", "both")]] []
  in
    div []
      [ turnView
      , hr
      , scoreView
      , hr
      , (case winner model of
          Nothing -> selectionView
          Just color -> gameOverView color
        )
      , hr
      , boardView]

palette =
  { gray      = "#E9E8E5"
  , altGray   = "#E6E5E2"
  , lightRed  = "#F7998D"
  , lightBlue = "#78C8F5"
  , red       = "#FF432E"
  , blue      = "#00A2FF"
  }

letterSize = 24

boardStyle =
  [ ("width", "120px")
  , ("position", "relative")
  ]

letterStyle letter =
  let background =
    case letter.color of
      Just Blue ->
        (if letter.locked then .blue else .lightBlue) palette

      Just Red ->
        (if letter.locked then .red  else .lightRed)  palette

      Nothing ->
        palette.gray
  in
    [ ("display", "block")
    , ("width",  (toString letterSize) ++ "px")
    , ("height", (toString letterSize) ++ "px")
    , ("line-height", (toString letterSize) ++ "px")
    , ("text-align", "center")
    , ("background", background)
    ]

boardLetterStyle letter =
  [ ("visibility", if .selected letter then "hidden" else "visible")
  , ("position", "absolute")
  , ("top",  (toString (letterSize * letter.y)) ++ "px")
  , ("left", (toString (letterSize * letter.x)) ++ "px")
  ]

selectionLetterStyle =
  [ ("float", "left")
  ]

letterView address letter additionalStyle action =
  div
    [ style ((letterStyle letter) ++ additionalStyle)
    , onClick address action
    ]
    [text (String.fromChar letter.char)]

selectedLetterView address letter =
  letterView address letter selectionLetterStyle (Unselect letter)

boardLetterView address letter =
  letterView address letter (boardLetterStyle letter) (Select letter)
