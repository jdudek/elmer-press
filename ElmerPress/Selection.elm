module ElmerPress.Selection where

import ElmerPress.Letter as Letter exposing (Letter)
import Char
import String

type alias Selection = List Letter

addLetter letter selection =
  if List.member letter selection then selection else selection ++ [letter]

removeLetter letter selection =
  List.filter (\l -> l /= letter) selection

member letter selection =
  List.member letter selection

toWord : Selection -> String
toWord selection =
  String.fromList (List.map (Char.toLower << .char) selection)
