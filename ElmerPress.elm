module ElmerPress where

import Html exposing (div, span, text, button)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import StartApp
import String
import Array
import Debug
import Random
import Time
import Signal.Extra

main =
  let
    actions =
      Signal.mailbox Nothing

    address =
      Signal.forwardTo actions.address Just

    inputs = Signal.map2 (,) actions.signal seeds

    models =
      let
        updateFromInput ((Just action), seed) model = update action model
      in
        Signal.Extra.foldp' updateFromInput (newModel << snd) inputs
  in
    Signal.map (view address) models

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

-- update

type Action = Select Letter | Unselect Letter | Submit

addLetterToSelection letter selection =
  if List.member letter selection then selection else selection ++ [letter]

removeLetterFromSelection letter selection =
  List.filter (\l -> l /= letter) selection

memberOfSelection letter selection =
  List.member letter selection

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

update action model =
  if isGameOver model then model
  else case action of
    Select letter ->
      let newLetter = { letter | selected <- True }
          newBoard  = replaceLetter (.board model) letter newLetter
          newSelection = addLetterToSelection newLetter (.selection model)
      in
        { model | board <- newBoard, selection <- newSelection }
    Unselect letter ->
      let newLetter = { letter | selected <- False }
          newBoard  = replaceLetter (.board model) letter newLetter
          newSelection = removeLetterFromSelection letter (.selection model)
      in
        { model | board <- newBoard, selection <- newSelection }

    Submit ->
      let markColor letter =
            if memberOfSelection letter (.selection model) && not (.locked letter)
              then { letter | color <- Just (.turn model), selected <- False }
              else { letter | selected <- False }
          hasColor color letter = case color of
            Just color -> .color letter == Just color
            Nothing    -> False
          markLocked board letter =
            if List.all (hasColor (.color letter)) (neighboursOf letter board)
              then { letter | locked <- True }
              else { letter | locked <- False }
          coloredBoard = List.map (markColor) (.board model)
          newBoard = List.map (markLocked coloredBoard) coloredBoard
      in
        { model | board <- newBoard, selection <- [], turn <- flipColor (.turn model) }

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
        button [onClick address Submit] [text "Submit"]
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
