module ElmerPress where

import Html exposing (div, span, text, button)
import Html.Attributes exposing (style, disabled)
import Html.Events exposing (onClick)
import StartApp
import String
import Array
import Debug
import Random
import Time
import Signal.Extra
import Http
import Char
import Task exposing (Task, succeed, fail, andThen, onError)

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
seeds = Signal.map (Random.initialSeed << round << ((*) 1000) << fst) (Time.timestamp (Signal.constant ()))

type Color = Red | Blue
type alias Letter = { x: Int, y: Int, char: Char, color: Maybe Color, selected: Bool, locked: Bool }
type alias Board = List Letter
type alias Model = { board : Board, selection: List Letter, turn: Color }

flipColor color =
  case color of
    Red  -> Blue
    Blue -> Red

hasColor color letter =
  case color of
    Just color -> letter.color == Just color
    Nothing    -> False

randomLetters : Random.Seed -> List Char
randomLetters seed =
  let alphabet = Array.fromList
        [ 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M'
        , 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z']
      intGenerator  = Random.int 0 (Array.length alphabet - 1)
      listGenerator = Random.list 25 intGenerator
      numbers = fst (Random.generate listGenerator seed)
      alphabetAt = (flip (Array.get) alphabet) >> (\(Just v) -> v)
  in List.map alphabetAt numbers

product : List a -> List b -> List (a, b)
product xs ys =
  let product' xs ys =
    case xs of
      x::xs' -> (List.map (\y -> (x, y)) ys)::(product' xs' ys)
      []     -> []
  in List.concat (product' xs ys)

boardFromLetters letters =
  let newLetter (x, y) char = { x = x, y = y, char = char, color = Nothing, selected = False, locked = False }
      coords = product [0,1,2,3,4] [0,1,2,3,4]
  in List.map2 newLetter coords letters

newModel : Random.Seed -> Model
newModel seed =
  let
    board = boardFromLetters (randomLetters seed)
  in
    { board = board, selection = [], turn = Red }

replaceLetter board letter newLetter =
  let replace xs y z =
        case xs of
          x::xs' -> if x == y then z::xs' else x::(replace xs' y z)
          []     -> []
  in
    replace board letter newLetter

toggleSelected letter =
  { letter | selected <- not (.selected letter) }

letterAt x y board =
  List.head (List.filter (\letter -> .x letter == x && .y letter == y) board)

compact list =
  let maybeToList item = case item of
        Just letter -> [letter]
        Nothing     -> []
  in List.concatMap maybeToList list

neighboursOf letter board =
  let before = letterAt (.x letter - 1) (.y letter) board
      after  = letterAt (.x letter + 1) (.y letter) board
      above  = letterAt (.x letter) (.y letter - 1) board
      below  = letterAt (.x letter) (.y letter + 1) board
  in compact [before, after, above, below]

countLettersOfColor color board =
  List.length (List.filter (\l -> .color l == (Just color)) board)

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

addLetterToSelection letter selection =
  if List.member letter selection then selection else selection ++ [letter]

removeLetterFromSelection letter selection =
  List.filter (\l -> l /= letter) selection

memberOfSelection letter selection =
  List.member letter selection

wordFromSelection : List Letter -> String
wordFromSelection selection =
  String.fromList (List.map (Char.toLower << .char) selection)

isCorrectWord : String -> List (String, Bool) -> Bool
isCorrectWord word words =
  List.any (\(word', status) -> word == word' && status == True) words

winner model =
  let numberOfBlue = countLettersOfColor Blue (.board model)
      numberOfRed  = countLettersOfColor Red (.board model)
  in
    if numberOfRed + numberOfBlue == List.length (.board model)
      then if numberOfBlue > numberOfRed then Just Blue else Just Red
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
      replaceLetter model.board letter newLetter

    newSelection =
      addLetterToSelection newLetter model.selection
  in
    { model | board <- newBoard, selection <- newSelection }

unselectLetter letter model =
  let
    newLetter =
      { letter | selected <- False }

    newBoard =
      replaceLetter model.board letter newLetter

    newSelection =
      removeLetterFromSelection letter model.selection
  in
    { model | board <- newBoard, selection <- newSelection }

switchTurn model =
  let
    markLetterColor letter =
      if memberOfSelection letter model.selection && not letter.locked
        then { letter | color <- Just model.turn, selected <- False }
        else { letter | selected <- False }

    markIfLetterLocked board letter =
      let
        neighbours =
          neighboursOf letter board
      in
        { letter | locked <- List.all (hasColor letter.color) neighbours }

    markLettersColors board =
      List.map (markLetterColor) board

    markIfLettersLocked board =
      List.map (markIfLetterLocked board) board

    newBoard =
      (markIfLettersLocked << markLettersColors) model.board
  in
    { model | board <- newBoard, selection <- [], turn <- flipColor model.turn }

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
        if isCorrectWord (wordFromSelection model.selection) words
        then switchTurn model
        else model

-- view

view address model =
  let turnView =
        div []
          [ text "Current turn: "
          , text (toString (.turn model))
          ]
      scoreView =
        div []
          [ text "Red: "
          , text (toString (countLettersOfColor Red (.board model)))
          , text " "
          , text "Blue: "
          , text (toString (countLettersOfColor Blue (.board model)))
          ]
      submitButton =
        button
          [ onClick wordQueries.address (wordFromSelection model.selection)
          , disabled (List.isEmpty model.selection)
          ]
          [text "Submit"]
      selectionView =
        div []
          ((List.map (selectedLetterView address) (.selection model)) ++ [submitButton])
      boardView =
        div [style boardStyle]
          (List.map (boardLetterView address) (.board model))
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
  [ ("display", "block")
  , ("width",  (toString letterSize) ++ "px")
  , ("height", (toString letterSize) ++ "px")
  , ("line-height", (toString letterSize) ++ "px")
  , ("text-align", "center")
  , ("background", case .color letter of
                     Just Blue -> (if (.locked letter) then .blue else .lightBlue) palette
                     Just Red  -> (if (.locked letter) then .red  else .lightRed)  palette
                     Nothing   -> .gray palette)
  ]

boardLetterStyle letter =
  [ ("visibility", if .selected letter then "hidden" else "visible")
  , ("position", "absolute")
  , ("top",  (toString (letterSize * (.y letter))) ++ "px")
  , ("left", (toString (letterSize * (.x letter))) ++ "px")
  ]

selectionLetterStyle =
  [ ("float", "left")
  ]

letterView address letter additionalStyle action =
  div
    [ style ((letterStyle letter) ++ additionalStyle)
    , onClick address action
    ]
    [text (String.fromChar (.char letter))]

selectedLetterView address letter =
  letterView address letter selectionLetterStyle (Unselect letter)

boardLetterView address letter =
  letterView address letter (boardLetterStyle letter) (Select letter)
