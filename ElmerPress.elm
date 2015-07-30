module ElmerPress where

import Html exposing (div, span, text, button)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import StartApp
import String
--import Maybe

main =
  StartApp.start { model = model, view = view, update = update }

type Color = Red | Blue
type alias Letter = { char: Char, color: Maybe Color, selected: Bool }
type alias Board = List Letter
type alias Model = { board : Board, selection: List Letter, turn: Color }

newLetter char = { char = char, color = Nothing, selected = False }

flipColor color =
  case color of
    Red  -> Blue
    Blue -> Red

board =
  [ newLetter 'A', newLetter 'B', newLetter 'C', newLetter 'D', newLetter 'E'
  , newLetter 'F', newLetter 'G', newLetter 'H', newLetter 'I', newLetter 'J'
  , newLetter 'K', newLetter 'L', newLetter 'M', newLetter 'N', newLetter 'O'
  , newLetter 'P', newLetter 'Q', newLetter 'R', newLetter 'S', newLetter 'T'
  , newLetter 'U', newLetter 'V', newLetter 'W', newLetter 'X', newLetter 'Y'
  ]

model : Model
model = { board = board, selection = [], turn = Red }

--at : Int -> List a -> Maybe a
--at i xs = List.head (List.drop (i - 1) xs)

--indexOf : a -> List a -> Maybe Int
--indexOf x xs =
--  case xs of
--    []      -> Nothing
--    x'::xs' ->
--      if x == x' then Just 0
--      else case indexOf x xs' of
--        Just n  -> Just (n + 1)
--        Nothing -> Nothing

--letterAt row column board =
--  itemAt column (itemAt row board)

--selectLetterAt : Int -> Board -> Board
--selectLetterAt idx board =
--  let before = List.take (idx - 1) board
--      after  = List.drop (idx) board
--      item   = at idx board
--  in
--    case item of
--      Just letter -> before ++ [{ letter | selected <- True }] ++ after
--      Nothing     -> board

--indexOfLetter letter board =
--  indexOf
--  if List.member letter board then Just letter else Nothing

replaceLetter board letter newLetter =
  let replace xs y z =
        case xs of
          x::xs' -> if x == y then z::xs' else x::(replace xs' y z)
          []     -> []
  in
    replace board letter newLetter

toggleSelected letter =
  { letter | selected <- not (.selected letter) }

letterBefore letter board =
  let findBefore ls =
        case ls of
          l::l'::ls' -> if l' == letter then Just l else findBefore (l'::ls')
          l::[]      -> Nothing
          []         -> Nothing
  in findBefore board

letterAfter letter board =
  let findAfter ls =
        case ls of
          l::ls' -> if l == letter then List.head ls' else findAfter ls'
          []     -> Nothing
  in findAfter board

--neighbours board letter =
--  let before
--      after
--      above
--      below

-- update

type Action = Select Letter | Unselect Letter | Submit

--selectLetter letter model =
--  let newLetter = toggleSelected letter
--      newBoard  = replaceLetter (.board model) letter newLetter
--      newSelection = newLetter::(.selection model)
--  in
--    { model | board <- newBoard }

--unselectLetter letter model =

addLetterToSelection letter selection =
  if List.member letter selection then selection else selection ++ [letter]

removeLetterFromSelection letter selection =
  List.filter (\l -> l /= letter) selection

memberOfSelection letter selection =
  List.member letter selection

update action model =
  case action of
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
            if memberOfSelection letter (.selection model)
              then { letter | color <- Just (.turn model), selected <- False }
              else letter
          newBoard = List.map markColor (.board model)
      in
        { model | board <- newBoard, selection <- [], turn <- flipColor (.turn model) }

-- view

view address model =
  let turnView =
        div [] [text (toString (.turn model))]
      selectionView =
        div [] --[style selectionStyle]
          (List.map (selectedLetterView address) (.selection model))
      submitButton =
        button [onClick address Submit] [text "Submit"]
      boardView =
        div [style boardStyle]
          (List.map (boardLetterView address) (.board model))
      hr = Html.hr [style [("clear", "both")]] []
  in
    div []
      [ turnView
      , hr
      , selectionView
      , submitButton
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

boardStyle =
  [ ("width", "120px")
  ]

letterStyle letter =
  [ ("display", "block")
  , ("float", "left")
  , ("width", "24px")
  , ("height", "24px")
  , ("text-align", "center")
  , ("line-height", "24px")
  ]

boardLetterStyle letter =
  [ ("visibility", if .selected letter then "hidden" else "visible")
  , ("background", case .color letter of
                     Just Blue -> .lightBlue palette
                     Just Red  -> .lightRed  palette
                     Nothing   -> .gray palette)
  ]

letterView address letter additionalStyle action =
  div
    [ style ((letterStyle letter) ++ additionalStyle)
    , onClick address action
    ]
    [text (String.fromChar (.char letter))]

selectedLetterView address letter =
  letterView address letter [] (Unselect letter)

  --div
  --  [ style (letterStyle letter)
  --  --, onClick address (Unselect letter)
  --  ]
  --  [text (String.fromChar (.char letter))]

boardLetterView address letter =
  letterView address letter (boardLetterStyle letter) (Select letter)

  --div
  --  [ style (letterStyle letter) ++ (boardLetterStyle letter)
  --  , onClick address (Select letter)
  --  ]
  --  [text (String.fromChar (.char letter))]
