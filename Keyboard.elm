module Keyboard exposing (..)

import Types exposing (..)

import Dict exposing (Dict, insert, get)
import Set exposing (Set, member, isEmpty, fromList)
import Html.Styled exposing (Html, fromUnstyled)
import Color exposing (..)
import Collage exposing (..)
import Collage.Events as CE exposing (..)
import Collage.Layout as CL exposing (..)
import Collage.Text as CT exposing (..)
import Collage.Render exposing (..)

--wrapper for parsing letters into characters (see below)
parseChars : List String -> (String, List Int)
parseChars input = let (revs, revi) = parseCharList [] [] input in 
  (String.concat (List.reverse revs), List.reverse revi)

--recursively parses a string of individual hangul letters and returns a valid list of 
--composed hangul characters and number of letters per character
parseCharList : List String -> List Int -> List String -> (List String, List Int)
parseCharList accs acci input = case input of 
  [] -> (accs, acci)
  c1 :: rest1 -> case rest1 of 
    [] -> (c1 :: accs, 1 :: acci)
    c2 :: rest2 -> if not (Dict.member c2 medialDict)
      then if finalCompatible c1 c2
        then parseCharList ((finalMap c1 c2) :: accs) (2 :: acci) rest2
        else parseCharList (c1 :: accs) (1 :: acci) rest1
      else if Dict.member c1 medialDict 
        then if (medialCompatible c1 c2)
          then parseCharList ((medialMap c1 c2) :: accs) (2 :: acci) rest2
          else parseCharList (c1 :: accs) (1 :: acci) rest1
        else case rest2 of 
          [] -> ((combine [c1, c2]) :: accs, 2 :: acci)
          c3 :: rest3 -> if Dict.member c3 medialDict 
            then if not (medialCompatible c2 c3)
              then parseCharList ((combine [c1,c2]) ::accs) (2 ::acci) rest2
              else case rest3 of 
                [] -> ((combine [c1,c2,c3]) :: accs, 3 :: acci) 
                c4 :: rest4 -> if Dict.member c4 medialDict
                  then parseCharList ((combine [c1,c2, c3]) :: accs) (3 :: acci) rest3
                  else case rest4 of 
                    [] -> (combine [c1,c2,c3,c4] :: accs, 4 :: acci)
                    c5 :: rest5 -> if Dict.member c5 medialDict 
                      then parseCharList ((combine [c1,c2,c3]) ::accs) (3 ::acci) rest3 
                      else if finalCompatible c4 c5  
                        then case rest5 of 
                          [] -> (combine [c1,c2,c3,c4,c5] :: accs, 5 ::acci)
                          c6 :: _ -> if Dict.member c6 medialDict
                            then parseCharList ((combine [c1,c2,c3,c4]) ::accs) (4 ::acci) rest4 
                            else parseCharList ((combine [c1,c2,c3,c4,c5]) ::accs) (5 ::acci) rest5
                        else parseCharList ((combine [c1,c2,c3,c4]) ::accs) (4 ::acci) rest4 
            else case rest3 of 
              [] -> ((combine [c1,c2,c3]) :: accs, 3 :: acci) 
              c4 :: rest4 -> if Dict.member c4 medialDict
                then parseCharList ((combine [c1,c2]) :: accs) (2 :: acci) rest2
                else if finalCompatible c3 c4 
                  then case rest4 of 
                    [] -> (combine [c1,c2,c3,c4] :: accs, 4 :: acci)
                    c5 :: _ -> if Dict.member c5 medialDict 
                      then parseCharList ((combine [c1,c2,c3]) ::accs) (3 ::acci) rest3 
                      else parseCharList ((combine [c1,c2,c3,c4]) ::accs) (4 ::acci) rest4 
                  else parseCharList ((combine [c1,c2,c3]) ::accs) (3 ::acci) rest3 

dWD : (Dict String Int) -> String -> Int 
dWD d s = Maybe.withDefault 0 (get s d)

--these are all just rules for commposition for Hangul

medialMap : String -> String -> String 
medialMap s1 s2 = case (s1, s2) of 
  ("ㅜ", "ㅣ") -> "ㅟ"
  ("ㅜ", "ㅓ") -> "ㅝ"
  ("ㅗ", "ㅣ") -> "ㅚ"
  ("ㅗ", "ㅏ") -> "ㅘ"
  ("ㅡ", "ㅣ") -> "ㅢ"
  ("ㅜ", "ㅔ") -> "ㅞ"
  ("ㅗ", "ㅐ") -> "ㅙ"
  _ -> ""

medialCompatible : String -> String -> Bool 
medialCompatible s1 s2 = case (s1,s2) of 
  ("ㅜ", "ㅣ") -> True
  ("ㅜ", "ㅓ") -> True
  ("ㅗ", "ㅣ") -> True
  ("ㅗ", "ㅏ") -> True
  ("ㅡ", "ㅣ") -> True
  ("ㅜ", "ㅔ") -> True
  ("ㅗ", "ㅐ") -> True
  _ -> False

finalMap : String -> String -> String 
finalMap s1 s2 = case (s1, s2) of 
  ("ㄱ", "ㅅ") -> "ㄳ"
  ("ㄴ", "ㅈ") -> "ㄵ"
  ("ㄴ", "ㅎ") -> "ㄶ"
  ("ㄹ", "ㄱ") -> "ㄺ"
  ("ㄹ", "ㅁ") -> "ㄻ"
  ("ㄹ", "ㅂ") -> "ㄼ"
  ("ㄹ", "ㅅ") -> "ㄽ"
  ("ㄹ", "ㅌ") -> "ㄾ"
  ("ㄹ", "ㅍ") -> "ㄿ"
  ("ㄹ", "ㅎ") -> "ㅀ"
  ("ㅂ", "ㅅ") -> "ㅄ"
  _ -> ""

finalCompatible : String -> String -> Bool 
finalCompatible s1 s2 = case (s1,s2) of 
  ("ㄱ", "ㅅ") -> True
  ("ㄴ", "ㅈ") -> True
  ("ㄴ", "ㅎ") -> True
  ("ㄹ", "ㄱ") -> True
  ("ㄹ", "ㅁ") -> True
  ("ㄹ", "ㅂ") -> True
  ("ㄹ", "ㅅ") -> True
  ("ㄹ", "ㅌ") -> True
  ("ㄹ", "ㅍ") -> True
  ("ㄹ", "ㅎ") -> True
  ("ㅂ", "ㅅ") -> True
  _ -> False

--combines a list of hangul letters into a valid character
combine : List String -> String 
combine chars = case chars of 
  c1 :: [] -> c1
  c1 :: c2 :: [] -> ((dWD initialDict c1) * 588 + (dWD medialDict c2) * 28) + 44032
    |> Char.fromCode
    |> String.fromChar
  c1 :: c2 :: c3 :: [] -> if medialCompatible c2 c3 
    then ((dWD initialDict c1) * 588 + (dWD medialDict (medialMap c2 c3)) * 28) + 44032
      |> Char.fromCode 
      |> String.fromChar
    else ((dWD initialDict c1) * 588 + (dWD medialDict c2) * 28 + (dWD finalDict c3)) + 44032
      |> Char.fromCode 
      |> String.fromChar
  c1 :: c2 :: c3 :: c4 :: [] -> if medialCompatible c2 c3
    then ((dWD initialDict c1) * 588 
        + (dWD medialDict (medialMap c2 c3)) * 28 
        + (dWD finalDict c4))
        + 44032
      |> Char.fromCode 
      |> String.fromChar
    else if finalCompatible c3 c4
      then ((dWD initialDict c1) * 588 
          + (dWD medialDict c2) * 28 
          + (dWD finalDict (finalMap c3 c4)))
          + 44032
        |> Char.fromCode 
        |> String.fromChar
      else ""
  c1 :: c2 :: c3 :: c4 :: c5 :: [] -> if not (medialCompatible c2 c3 && (finalCompatible c4 c5))
    then ""
    else  ((dWD initialDict c1) * 588 
          + (dWD medialDict (medialMap c2 c3)) * 28 
          + (dWD finalDict (finalMap c4 c5)))
          + 44032
      |> Char.fromCode 
      |> String.fromChar
  _ -> ""

--draws the keyboard
renderKeyboard : Keyboard -> Html Msg
renderKeyboard kb =
  let 
    validKeys = kb.validKeys
    shift = kb.shift
    allStrings = case shift of
      On ->  [topRowShifted, middleRow, bottomRow]
      Off ->  [topRow, middleRow, bottomRow]
  in List.map (List.map (\key -> if member key validKeys 
                    then key
                    else "")) allStrings 
    |> List.map (List.map renderKey)
    |> List.map CL.horizontal
    |> List.map CL.center
    |> CL.vertical
    |> svg 
    |> fromUnstyled

--draws one key
renderKey : String -> Collage Msg  
renderKey letter = 
  let 
    korText = fromString letter
      |> typeface (Font "Malgun Gothic")
      |> size enormous
      |> weight Bold
      |> CT.color (rgb255 71 69 76)
      |> Collage.rendered
    cardRect = Collage.image (50, 50) "assets/key.png"
      |> at base korText 
      |> CE.onClick (KeyPress letter)
  in --Adding a border
    Collage.rectangle 56 56
    |> filled (uniform white)
    |> at base cardRect

topRow : List String
topRow = List.append (String.split "" "ㅂㅈㄷㄱㅅㅛㅕㅑㅐㅔ") ["Back"]

topRowShifted : List String
topRowShifted = List.append (String.split "" "ㅃㅉㄸㄲㅆㅛㅕㅑㅒㅖ") ["Back"]

middleRow : List String
middleRow = String.split "" "ㅁㄴㅇㄹㅎㅗㅓㅏㅣ"

bottomRow : List String
bottomRow = List.append (String.split "" "ㅋㅌㅊㅍㅠㅜㅡ") ["Shift"]

allLetters : Set String
allLetters = List.concat [topRow, topRowShifted, middleRow, bottomRow]
  |> Set.fromList 

initialDict : Dict String Int 
initialDict = Dict.empty 
  |> insert "ᄀ" 0   
  |> insert "ㄸ" 4   
  |> insert "ㅃ" 8   
  |> insert "ㅈ" 12  
  |> insert "ㅌ" 16 
  |> insert "ㄲ" 1   
  |> insert "ㄹ" 5   
  |> insert "ㅅ" 9   
  |> insert "ㅉ" 13  
  |> insert "ㅍ" 17 
  |> insert "ㄴ" 2   
  |> insert "ㅁ" 6   
  |> insert "ㅆ" 10  
  |> insert "ㅊ" 14  
  |> insert "ㅎ" 18 
  |> insert "ㄷ" 3   
  |> insert "ㅂ" 7   
  |> insert "ㅇ" 11  
  |> insert "ㅋ" 15  

medialDict : Dict String Int 
medialDict = Dict.empty
  |> insert "ㅏ" 0
  |> insert "ㅖ" 7
  |> insert "ㅝ" 14
  |> insert "ㅐ" 1
  |> insert "ㅗ" 8
  |> insert "ㅞ" 15
  |> insert "ㅑ" 2
  |> insert "ㅘ" 9
  |> insert "ㅟ" 16
  |> insert "ㅒ" 3
  |> insert "ㅙ" 10
  |> insert "ㅠ" 17
  |> insert "ㅓ" 4
  |> insert "ㅚ" 11
  |> insert "ㅡ" 18
  |> insert "ㅔ" 5
  |> insert "ㅛ" 12
  |> insert "ㅢ" 19
  |> insert "ㅕ" 6 
  |> insert "ㅜ" 13  
  |> insert "ㅣ" 20

finalDict : Dict String Int 
finalDict = Dict.empty
  |> insert "ㄷ" 7
  |> insert "ㄿ" 14
  |> insert "ㅇ" 21
  |> insert "ㄱ" 1 
  |> insert "ㄹ" 8
  |> insert "ㅀ" 15
  |> insert "ㅈ" 22
  |> insert "ㄲ" 2 
  |> insert "ㄺ" 9
  |> insert "ㅁ" 16
  |> insert "ㅊ" 23
  |> insert "ㄳ" 3 
  |> insert "ㄻ" 10
  |> insert "ㅂ" 17
  |> insert "ㅋ" 24
  |> insert "ㄴ" 4 
  |> insert "ㄼ" 11
  |> insert "ㅄ" 18
  |> insert "ㅌ" 25
  |> insert "ㄵ" 5 
  |> insert "ㄽ" 12
  |> insert "ㅅ" 19
  |> insert "ㅍ" 26
  |> insert "ㄶ" 6 
  |> insert "ㄾ" 13  
  |> insert "ㅆ" 20  
  |> insert "ㅎ" 27