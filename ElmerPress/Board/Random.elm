module ElmerPress.Board.Random (init) where

import ElmerPress.Board as Board
import Random exposing (Generator, Seed)

init seed =
  Board.init (randomLetters seed)

-- private

randomLetters seed =
  let
    intervals' =
      intervals probabilites

    randoms =
      fst (take 25 randomFloat seed)
  in
    List.map ((\(Just letter) -> letter) << findLetter intervals') randoms

probabilites : List (Char, Float)
probabilites =
  [ ('a', 0.07716940430787371)
  , ('h', 0.024906742076918773)
  , ('e', 0.11276425705375108)
  , ('d', 0.03307395296159421)
  , ('i', 0.09103100348126132)
  , ('n', 0.06746490562290303)
  , ('g', 0.027565174145822054)
  , ('s', 0.09639200405424683)
  , ('l', 0.05234068914314628)
  , ('r', 0.06968788446972832)
  , ('v', 0.009235549061194379)
  , ('k', 0.00892462363837127)
  , ('w', 0.0073498986832367455)
  , ('o', 0.06675320560564253)
  , ('f', 0.011711524259670411)
  , ('t', 0.06579639074301324)
  , ('b', 0.01828864125196151)
  , ('c', 0.04039350784564543)
  , ('y', 0.01627925122537719)
  , ('u', 0.03289031386522467)
  , ('m', 0.02895389555270245)
  , ('p', 0.030112082900835675)
  , ('x', 0.002797146579465681)
  , ('j', 0.001616102863114787)
  , ('z', 0.0048467322880879704)
  , ('q', 0.0016551163192104622)
  ]

intervals : List (Char, Float) -> List (Char, Float, Float)
intervals ((firstLetter, firstProbability)::probabilities) =
  let
    next (letter, probability) ((previous, from, to)::intervals) =
      (letter, to, to + probability)::(previous, from, to)::intervals
  in
    List.foldl next [(firstLetter, 0, firstProbability)] probabilities

findLetter : List (Char, Float, Float) -> Float -> Maybe Char
findLetter intervals x =
  let
    matches =
      List.filter (\(letter, from, to) -> from <= x && x < to) intervals
  in
    case List.head matches of
      Just (char, _, _) -> Just char
      Nothing -> Nothing

{-| Generator that produces floats in [0, 1) range.
-}
randomFloat : Generator Float
randomFloat =
  Random.customGenerator <| \seed ->
    let
      (number, seed') =
        Random.generate (Random.int 0 (Random.maxInt - 1)) seed

      scaled =
        toFloat number / toFloat Random.maxInt
    in
      (scaled, seed')

take : Int -> Generator a -> Seed -> (List a, Seed)
take n g s =
  if n > 0 then
    let
      (a, s') = Random.generate g s
      (tail, s'') = take (n - 1) g s'
    in
      (a :: tail, s'')
  else
    ([], s)
