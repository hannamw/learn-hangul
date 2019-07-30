module Types exposing (..)

import Css exposing (..)
import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes as HA exposing (css, href, src, autoplay, controls, style, rel)
import Html.Styled.Events exposing (onClick)
import Set exposing (Set)
import Hex

type Msg  = CardClicked KoreanName
          | Forward
          | Backward
          | ChangeModel Model
          | NewInput String
          | InputSubmitted
          | KeyPress String
          | MatchClick String

type alias KoreanName = String
type alias Meaning = String
type alias RomanizedTranscription = String 
type Card = Card KoreanName Meaning RomanizedTranscription
type Side = Hangul | Romanization

type ShiftState = On | Off 
type alias Keyboard =   { validKeys : Set String
                        , shift : ShiftState}

type alias Section =  { name : String 
                      , subsections : List Subsection
                      }

--see Sections.elm
type alias Subsection = { name : String 
                        , update : Msg -> Model -> (Model, Cmd Msg)
                        , view : Model -> Html Msg
                        , defaultState : State
                        , loadFunction : Model -> Cmd Msg
                        }

--each subsection is either a lesson or type of game
type State  = Lesson
            | AudioTextGame AudioTextGameState
            | TranscriptionGame TranscriptionGameState
            | KorTransGame KorTransGameState
            | TransMatchGame TransMatchGameState


type alias AudioTextGameState = { numCorrect : Int 
                         , correct : Set String 
                         , incorrect : Set String 
                         , questions : List Card
                         }
                         
type alias TranscriptionGameState = { questions : List Card 
                                    , answeredQuestions : List Card
                                    , input : String}

type alias KorTransGameState =   { questions : List Card 
                                , answeredQuestions : List Card
                                , rawInput : List String
                                , parsedInput : String 
                                , parsedLen : List Int
                                , keyboard : Keyboard}

type alias TransMatchGameState =    { questions : List (Card, Side)
                                    , correct : Set String 
                                    , currentCard : Maybe String}


-- MODEL
type Model = M
  { section : String
  , subsection : String 
  , state : State 
  , view : Model -> Html Msg
  , update : Msg -> Model -> (Model, Cmd Msg)
  , playSound : (Bool, Maybe String)
  }

---CSS Stylings---

titleText : Attribute Msg 
titleText = 
  css [ textAlign center
      , fontFamilies [ "Nanum Gothic", "sans-serif" ]
      , color (rgb 104 81 21)
      , fontSize (px 48)
      , fontWeight bold
      ]

keyboardDisplay : Attribute Msg 
keyboardDisplay = 
  css [ textAlign center
      , fontFamilies [ "Nanum Gothic", "sans-serif" ]
      , color (rgb 104 81 21)
      , fontSize (px 48)
      , fontWeight bold
      , height (px 48)
      ]

normalText : Attribute Msg 
normalText = 
  css [ textAlign center
      , fontFamilies ["Nanum Gothic", "sans-serif" ]
      , fontSize (px 20)
      ]

textSection : Attribute Msg 
textSection = 
  css [ backgroundColor (hexToColor "abe188")
      , borderRadius (px 25) 
      , width (px 800)
      , margin auto
      , padding2 (px 1) (px 8)
      ]

cardStyle : List Card -> Attribute Msg 
cardStyle cs = 
  let 
    lcs = List.length cs 
    cardWidth = 140
    w = if lcs > 4 then 4 * cardWidth else lcs * cardWidth
  in 
  css [ width (px <| toFloat w) 
      , margin auto
      ]

forwardStyle : Attribute Msg 
forwardStyle = 
  css [ width (px (158 * 2))
      , margin auto 
      ]

audioStyle : Attribute Msg 
audioStyle = 
  css [ width (px 300)
      , margin auto 
      ]

inputBlockStyle : Attribute Msg
inputBlockStyle = 
    css [ width (px 180) , margin auto] 

inputButtonStyle : Attribute Msg
inputButtonStyle = 
    css [ width (px 115) , margin auto] 

keyboardStyle : Attribute Msg
keyboardStyle = 
    css [ width (px 630) , margin auto] 

--maps a string representing a hex value to that color
hexToColor : String -> Color 
hexToColor s = 
  let 
    n1 = String.slice 0 2 s
    n2 = String.slice 2 4 s
    n3 = String.slice 4 6 s
  in case List.map Hex.fromString [n1, n2, n3] of
    Ok r :: Ok g :: Ok b :: [] -> rgb r g b
    _ -> Debug.todo "bad hex"

wrapper : List (Html Msg) -> Html Msg 
wrapper h = div [] h

soundFile : String -> String 
soundFile s = "assets/" ++ s ++ ".mp3"