module ElmerPress.Board
  ( Board
  , initRandom
  , replaceLetter
  , countLetters
  , countLettersOfColor
  , neighboursOf
  ) where

import ElmerPress.Letter as Letter exposing (Letter)
import Random
import Array

type alias Board = List Letter

initRandom : Random.Seed -> Board
initRandom seed =
  fromLetters (randomLetters seed)

replaceLetter board letter newLetter =
  let replace xs y z =
        case xs of
          x::xs' -> if x == y then z::xs' else x::(replace xs' y z)
          []     -> []
  in
    replace board letter newLetter

countLetters board =
  List.length board

countLettersOfColor color board =
  List.length (List.filter (\l -> .color l == (Just color)) board)

neighboursOf letter board =
  let before = letterAt (.x letter - 1) (.y letter) board
      after  = letterAt (.x letter + 1) (.y letter) board
      above  = letterAt (.x letter) (.y letter - 1) board
      below  = letterAt (.x letter) (.y letter + 1) board
  in compact [before, after, above, below]

-- private

letterAt x y board =
  List.head (List.filter (\letter -> .x letter == x && .y letter == y) board)

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

fromLetters letters =
  let
    newLetter (x, y) char =
      { x = x
      , y = y
      , char = char
      , color = Nothing
      , selected = False
      , locked = False
      }

    coords =
      product [0,1,2,3,4] [0,1,2,3,4]
  in
    List.map2 newLetter coords letters

compact list =
  let maybeToList item = case item of
        Just letter -> [letter]
        Nothing     -> []
  in List.concatMap maybeToList list
