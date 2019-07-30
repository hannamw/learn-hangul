module Sections exposing (..)

import Cards exposing (..)
import Navigation exposing (..)
import Types exposing (..)
import Keyboard exposing (..)

import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes as HA exposing  (css, href, src, autoplay, controls, style, rel
                                              , value, placeholder)
import Html.Styled.Events exposing (onClick, onInput)
import Set exposing (Set, empty, insert, fromList)
import Dict exposing (get, member)
import Random exposing (generate)
import Random.List exposing (shuffle)
import Task

--The meat of this project. Each page that you see in the final output is a subsection

{- Helpful reminder: 

type alias Section =  { name : String 
                      , subsections : List Subsection
                      , view : Model -> Html Msg 
                      }

type alias Subsection = { name : String 
                        , update : Msg -> Model -> (Model, Cmd Msg)
                        , view : Model -> Html Msg
                        , defaultState : State
                        , loadFunction : Model -> Cmd Msg
                        }

A subsection has a name, an update function, a default State, and a loadFunction
The default state tells us whether this is a lesson / some game, and what its default state is
the load function handles loading each subsection if it needs to call impure functions to do so

Each of these fields has its own section in this file.
-}

--------------------------
--- Update functions
--------------------------

--for Lessons
defaultUpdate : Msg -> Model -> (Model, Cmd Msg)
defaultUpdate msg ((M m) as model) =
  case msg of
    CardClicked k -> (setSound k model, Cmd.none)
    Forward -> (model, (forward sectionList model))
    Backward -> (model, (backward sectionList model))
    ChangeModel newModel -> (newModel, Cmd.none)
    _ -> (model, Cmd.none)

audioTextGameUpdate : Msg -> Model -> (Model, Cmd Msg)
audioTextGameUpdate msg ((M m) as model) =
  case msg of
    CardClicked k -> (gradeCard model k, Cmd.none)
    Forward -> (model, (forward sectionList model))
    Backward -> (model, (backward sectionList model))
    ChangeModel newModel -> (newModel, Cmd.none)
    _ -> (model, Cmd.none)

transcriptionGameUpdate : Msg -> Model -> (Model, Cmd Msg)
transcriptionGameUpdate msg ((M m) as model) =
  case msg of
    NewInput s -> (updateInput s model, Cmd.none)
    InputSubmitted -> (gradeInput model, Cmd.none)
    Forward -> (model, (forward sectionList model))
    Backward -> (model, (backward sectionList model))
    ChangeModel newModel -> (newModel, Cmd.none)
    _ -> (model, Cmd.none)

korTransGameUpdate : Msg -> Model -> (Model, Cmd Msg)
korTransGameUpdate msg ((M m) as model) =
  case msg of
    KeyPress k -> (addKey k model, Cmd.none)
    InputSubmitted -> (gradeInput model, Cmd.none)
    Forward -> (model, (forward sectionList model))
    Backward -> (model, (backward sectionList model))
    ChangeModel newModel -> (newModel, Cmd.none)
    _ -> (model, Cmd.none)

transMatchGameUpdate : Msg -> Model -> (Model, Cmd Msg)
transMatchGameUpdate msg ((M m) as model) =
  case msg of
    MatchClick k ->(checkMatch k model, Cmd.none)
    Forward -> (model, (forward sectionList model))
    Backward -> (model, (backward sectionList model))
    ChangeModel newModel -> (newModel, Cmd.none)
    _ -> (model, Cmd.none)
-----------------------------------------------------------------------------------
--Helpers

--set the sound to be played to the given sound
setSound : String -> Model -> Model
setSound k (M m) = M { m | playSound = (not (Tuple.first (m.playSound)), Just (soundFile k))}

--checks if the clicked card (in the audiotextgame) was the right one, and updates game state
gradeCard : Model -> String -> Model
gradeCard ((M m) as model) s = case m.state  of 
  AudioTextGame state -> case state.questions of 
    [] -> model
    (Card name _ _) :: rest -> 
      let
        newState = if s == name 
          then AudioTextGame {state  | numCorrect = state.numCorrect + 1
                    , questions = rest
                    , correct = insert s state.correct
                    , incorrect = Set.empty}
          else AudioTextGame {state | incorrect = insert s state.incorrect}
      in 
        M { m | state = newState}
  _ -> model

--checks if the input (either via real or digital keyboard) was correct for the gamestate, and updates
gradeInput : Model -> Model 
gradeInput ((M m) as model) = 
  case m.state of 
    TranscriptionGame gstate -> case gstate.questions of 
      [] -> model 
      ((Card _ _ transcription) as q) :: rest -> if gstate.input == transcription
        then M {m | state = TranscriptionGame {gstate | questions = rest
                                                      , input = ""
                                                      , answeredQuestions = q :: gstate.answeredQuestions}}
        else M {m | state = TranscriptionGame {gstate | input = ""}
                  , playSound = (not (Tuple.first (m.playSound)), Just "assets/incorrect.wav")}
    KorTransGame gstate -> case gstate.questions of 
      [] -> model 
      ((Card ktrans _ _) as q) :: rest -> if gstate.parsedInput == ktrans
        then M {m | state = KorTransGame {gstate | questions = rest
                                                  , rawInput = []
                                                  , parsedInput = ""
                                                  , parsedLen = []
                                                  , answeredQuestions = q :: gstate.answeredQuestions}}
        else M {m | state = KorTransGame {gstate  | rawInput = []
                                                  , parsedInput = ""
                                                  , parsedLen = []}
                  , playSound = wrongSound model}
    _ -> model

--returns the playSound attribute for the "wrong" sound
wrongSound : Model -> (Bool, Maybe String)
wrongSound (M m) = (not (Tuple.first (m.playSound)), Just "assets/incorrect.wav")

--make the model play the "wrong" sound
setWrongSound : Model -> Model 
setWrongSound ((M m) as model) = M {m | playSound = wrongSound model}

--make sure the text box displays what's been typed
updateInput : String -> Model -> Model 
updateInput s ((M m) as model) = 
  case m.state of
    TranscriptionGame gstate -> M {m | state = TranscriptionGame {gstate | input = s}} 
    _ -> model

--deselect the current card (set it to Nothing)
deselect : Model -> Model 
deselect ((M m) as model) = 
  case m.state of 
    TransMatchGame gstate -> M {m | state = TransMatchGame {gstate | currentCard = Nothing}}
    _ -> model 

--given a card name if a card is not selected, select the card if the card has not already been matched
--otherwise, check if the card selected matches the one that was given 
--if the same card was clicked twice, deselect the card.
checkMatch : String -> Model -> Model
checkMatch s ((M m) as model) = 
  case m.state of 
    TransMatchGame gstate -> case gstate.currentCard of 
      Nothing -> if Set.member s gstate.correct
        then model 
        else M ({m | state = TransMatchGame {gstate | currentCard = Just s}})
      Just cc -> case (member s cardDict, member cc cardDict) of 
        (True, False) -> case get s cardDict of 
          Nothing -> model 
          Just (Card k _ transcription) -> if cc == transcription 
            then M ({m | state = TransMatchGame {gstate  | currentCard = Nothing 
                                         , correct = insert k gstate.correct
                                            |> insert cc }})
            else deselect model |> setWrongSound
        (False, True) -> case get cc cardDict of 
          Nothing -> model 
          Just (Card k _ transcription) -> if s == transcription 
            then M ({m | state = TransMatchGame {gstate  | currentCard = Nothing 
                                         , correct = insert k gstate.correct
                                            |> insert s }})
            else deselect model |> setWrongSound
        _ -> if s == cc 
          then deselect model 
          else deselect model |> setWrongSound
    _ -> model  

--get the last item of the list of ints
lastItem : List Int -> Int 
lastItem xs = case xs of 
  [] -> 0 
  x :: [] -> x 
  x :: rest -> lastItem rest

--like take, but specify the number to drop from the back
dropLast : Int -> List a -> List a 
dropLast n xs = let l = List.length xs in List.take (l - n) xs 

--on keypress either change the keyboard shiftstate, delete input (back), or add a new letter
addKey : String -> Model -> Model 
addKey s ((M m) as model) = 
  case m.state of 
    KorTransGame gstate -> case s of
      "Shift" -> 
        let 
          gk = gstate.keyboard
          newShift = case gstate.keyboard.shift of 
            On -> Off 
            Off -> On 
        in M {m | state = KorTransGame {gstate | keyboard = {gk | shift = newShift}}}
      "Back" -> 
        let 
          lastLen = lastItem gstate.parsedLen 
          rawInput = dropLast lastLen gstate.rawInput 
        in 
          M {m | state = KorTransGame {gstate | rawInput = rawInput
                                              , parsedLen = dropLast 1 gstate.parsedLen
                                              , parsedInput = String.slice 0 -1 gstate.parsedInput}}
      _ -> 
        let 
          newInput = List.append gstate.rawInput [s] 
          (newParse, newLens) = parseChars newInput
        in 
          M {m | state = KorTransGame {gstate | rawInput = newInput 
                                              , parsedInput = newParse
                                              , parsedLen = newLens}}
    _ -> model

-----------------------------
---View functions
-----------------------------

defaultLessonView : List String -> List Card -> Model -> Html Msg
defaultLessonView strings cards = 
  let 
    paragraphs = List.map (\s -> p [normalText] [text s]) strings 
    showCards = List.take 4 cards

  in \_ -> [ div [textSection] paragraphs
       , div [cardStyle cards] [displayCardsMonocolor White showCards]]
        |> wrapper


audioTextGameView : List String -> List Card -> Model -> Html Msg 
audioTextGameView displayText cards ((M m) as model) = 
  let 
    paragraphs = List.map (\s -> p [normalText] [text s]) displayText 
    state = case m.state of 
      AudioTextGame st -> st
      _ -> Debug.todo "not in listening game"  
    audioDiv  = case state.questions of 
      (Card k _ _) :: _ -> div [audioStyle] [audio [src (soundFile k), autoplay True, controls True] []]
      [] -> div [] []

  in [ div [textSection] paragraphs
       , audioDiv 
       , div [cardStyle cards] [displayCards state.correct state.incorrect cards]]
       |> wrapper

--this is an input block that users can type in, as well as a button to check the answer
inputBlock : String -> Html Msg 
inputBlock s = div [inputBlockStyle]
                   [input [ placeholder "Transcription", value s, onInput NewInput] []
                   , div [inputButtonStyle] [inputButton] ]

transcriptionGameView : List String -> Model -> Html Msg 
transcriptionGameView displayText ((M m) as model) =
  let 
    paragraphs = List.map (\s -> p [normalText] [text s]) displayText 
    state = case m.state of 
      TranscriptionGame st -> st
      _ -> Debug.todo "not in transcription game"  
    audioDiv  = case state.questions of 
      (Card k _ _) :: _ -> div [audioStyle] [audio [src (soundFile k), autoplay True, controls True] []]
      [] -> div [] []

  in [ div [textSection] paragraphs
       , audioDiv 
       , inputBlock state.input
       , div [cardStyle state.answeredQuestions] [displayCardsMonocolor Green state.answeredQuestions]]
       |> wrapper


korTransGameView : List String -> Set String -> Model -> Html Msg 
korTransGameView displayText validKeys ((M m) as model) =
  let 
    paragraphs = List.map (\s -> p [normalText] [text s]) displayText 
    state = case m.state of 
      KorTransGame st -> st
      _ -> Debug.todo "not in korean transcription game"  
    audioDiv  = case state.questions of 
      (Card k _ _) :: _ -> div [audioStyle] [audio [src (soundFile k), autoplay True, controls True] []]
      [] -> div [] []
    userText = if state.parsedInput == "" then "_" else state.parsedInput
  in [ div [textSection] paragraphs
       , audioDiv 
       , p [keyboardDisplay] [text state.parsedInput]
       , div [inputButtonStyle] [inputButton]
       , div [keyboardStyle] [renderKeyboard state.keyboard]
       , div [cardStyle state.answeredQuestions] [displayCardsMonocolor Green state.answeredQuestions]]
       |> wrapper

transMatchGameView : List String -> Model -> Html Msg 
transMatchGameView displayText ((M m) as model) =
  let 
    paragraphs = List.map (\s -> p [normalText] [text s]) displayText 
    state = case m.state of 
      TransMatchGame st -> st
      _ -> Debug.todo "not in transMatch game"  

  in [ div [textSection] paragraphs
       , div  [cardStyle (List.map Tuple.first state.questions)] 
              [displayDoubleSided state.currentCard state.correct state.questions]]
       |> wrapper
--------------------------------------------------
---States
-------------------------------------------------
--default gamestates for each game

defaultATGState : State 
defaultATGState = 
  AudioTextGame { numCorrect = 0
         , correct = Set.empty
         , incorrect = Set.empty
         , questions = []
         }

defaultTGState : State 
defaultTGState = TranscriptionGame {questions = [], answeredQuestions = [], input = ""}

defaultKTGState : Set String -> State 
defaultKTGState ss = KorTransGame {questions = []
                               , answeredQuestions = []
                               , rawInput = []
                               , parsedInput = ""
                               , parsedLen = []
                               , keyboard = {shift = Off, validKeys = ss}}

defaultTMGState : State 
defaultTMGState = TransMatchGame  { questions = [] 
                                  , correct = Set.empty
                                  , currentCard = Nothing}

--updates a state to include new cards
loadQs : State -> List Card -> State 
loadQs state cards = case state of 
    AudioTextGame gstate -> AudioTextGame {gstate | questions = cards}
    TranscriptionGame gstate -> TranscriptionGame {gstate | questions = cards}
    KorTransGame gstate -> KorTransGame {gstate | questions = cards}
    _ -> Debug.todo "can't load Qs into this state"

--updates a state to include
loadDSQs : State -> List (Card, Side) -> State 
loadDSQs state cards = case state of 
  TransMatchGame gstate -> TransMatchGame {gstate | questions = cards}
  _ -> Debug.todo "invalid state"

-------------------
--Load Functions
-------------------

--changes the model ... to the current model
updateModelIdentity : Model -> Cmd Msg
updateModelIdentity model = send (ChangeModel model)

--I don't like having to do this
--It just sends out a dummy message
send : msg -> Cmd msg
send msg =
  Task.succeed msg
  |> Task.perform identity

--randomizes a list of cards
updateModelGame : List Card -> Model -> Cmd Msg 
updateModelGame cs ((M m) as model) = generate
  (\rcs -> ChangeModel (M {m | state = loadQs m.state rcs})) (Random.List.shuffle cs)

--randomizes a list of cards and sides (for matching games w/ front and back)
updateModelMatchGame : List Card -> Model -> Cmd Msg 
updateModelMatchGame cs ((M m) as model) = 
  List.map (\c -> [(c, Hangul), (c, Romanization)]) cs 
    |> List.concat 
    |> Random.List.shuffle
    |> generate (\rcs -> ChangeModel (M {m | state = loadDSQs m.state rcs}))
-----------------------------------------
--Sections
-------------------------------------------

--the masterlist of sections. if the section isn't in here, it's not in the game
sectionList : List Section 
sectionList = [section1, section2, sectionSandbox]

--the below functions make subsections of the given type

ssLesson : String -> List String -> List Card -> Subsection 
ssLesson s ss cs = { name = s
    , update = defaultUpdate
    , view = defaultLessonView ss cs
    , defaultState = Lesson 
    , loadFunction = updateModelIdentity
    }

ssAudioTextGame : String -> List String -> List Card -> Subsection
ssAudioTextGame s ss cs = { name = s
    , update = audioTextGameUpdate
    , view = audioTextGameView ss cs
    , defaultState = loadQs defaultATGState cs
    , loadFunction = updateModelGame cs
  }

ssTranscriptionGame : String -> List String -> List Card -> Subsection
ssTranscriptionGame s ss cs = { name = s
        , update = transcriptionGameUpdate
        , view = transcriptionGameView ss 
        , defaultState = loadQs defaultTGState cs
        , loadFunction = updateModelGame cs}

ssTransMatchGame : String -> List String -> List Card -> Subsection 
ssTransMatchGame s ss cs = { name = s
        , update = transMatchGameUpdate
        , view = transMatchGameView ss 
        , defaultState = defaultTMGState
        , loadFunction = updateModelMatchGame cs}

ssKorTransGame : String -> List String -> Set String -> List Card -> Subsection
ssKorTransGame s ss chars cs = { name = s
        , update = korTransGameUpdate
        , view = korTransGameView ss chars
        , defaultState = loadQs (defaultKTGState chars) cs
        , loadFunction = updateModelGame cs}

--Below, find all the content for the game. I won't write comments for all of them
--If you play through the game, it should be more or less clear how each works.
-------------------
---Section 1
-------------------

section1 : Section 
section1 =  { name = "Lesson One: Basic Korean Vowels"
      , subsections = [ss11, ss12, ss13, ss14]
      }

ss11 : Subsection
ss11 =  ssLesson "The first two vowels: 아 and 어" ss11text ss11cards

ss11cards =  stringsToCards ["아", "어"]
ss11text = ["""Let's introduce the first two Korean letters! 
      They are ㅏ and ㅓ. The first is pronounced "a", 
      like in "bar", and the second, like the "au" in "caught". 
      You might notice that the tiles below actually
      show 아 and 어, notㅏ and ㅓ - don't worry about that
      too much; we'll explain that in the next section."""
      ,"""Click on each tile to play its sound!"""]

ss12 : Subsection
ss12 = ssLesson "The next two vowels: 오 and 우" ss12text ss12cards

ss12cards : List Card
ss12cards = stringsToCards [ "오", "우"]
ss12text = ["""Let's introduce the next two Korean letters! 
      They are ㅗ and ㅜ. ㅗ is pronounced "o", like the Spanish "o",
      similar to "boat", while ㅜ is pronounced like the "oo" in "food"."""
      ,"""Click on each tile to play its sound!"""]


ss13 : Subsection
ss13 = ssLesson "The last two vowels: 으 and 이" ss13text ss13cards

ss13cards : List Card
ss13cards = stringsToCards ["으", "이"]
ss13text = ["""Let's introduce the last two basic Korean vowels! 
      They are 으 and 이. ㅡ is pronounced "eu", like the "oo" in "food,
      but with unrounded lips, while ㅣ is pronounced like the "e" in "me"."""
      ,"""Click on each tile to play its sound!"""]

ss14cards = List.concat [ss11cards, ss12cards, ss13cards]
ss14 : Subsection 
ss14 =  ssAudioTextGame "Review: go over the letters we've learned!" ss14text ss14cards
ss14text =  ["Match each sound to the corresponding letter!"]  


-------------------
---Section 2
-------------------

section2 : Section 
section2 =  { name = "Lesson Two: Basic Korean Consonants"
      , subsections = [ss21, ss22, ss23, ss24, ss25, ss25e, ss26, ss27]
      }

ss21 = ssLesson "The first two consonants: ㅇ and ㄱ" ss21text ss21cards 
ss21text = ["""Now that we've learned all of the basic vowels, we're going to learn the basic consonants, 
ㅇ and ㄱ!
  ㄱ is very easy - makes the "k" sound at the start of words, and the "g" sound elsewhere. 
  The trick with Korean is that every consonant must be paired with a vowel, and vice-versa! 
  So, to write "ka", we write 가.
  But, consonants look different when paired with certain vowels. When we write "ko", it's written 고 
  (and "ku" is 구). 
  You can probably see that the distinction is between vertical vowels, like ㅏ and ㅓ, 
  and horizontal vowels like ㅗ and 
  ㅜ.""", """So what about ㅇ, which we've seen before? It turns out that ㅇ is the null consonant - it doesn't 
  make any sound! So when we want just the a/ㅏ sound, we just write 아. You can't write ㅏ on its own. 
  Also, there's no null vowel (in case you were wondering). """]
ss21cards = stringsToCards ["가", "거", "고", "구", "그", "기"]

ss22 = ssLesson "More consonants: ㄴ and ㄷ" ss22text ss22cards 
ss22text = ["""Now that we've learned all of the basic vowels, we're going to learn the basic consonants, 
  ㄴ and ㄷ! ㄴ is exactly like the English letter "n"!""",
  """ ㄷ is most similar to the English letters "t" and "d". It makes the "t" sound at the start of words, 
  and the  "d" sound elsewhere. """,
  """ Just like the two letters that came before, ㄴ and ㄷ must always come with a consonant. 
    Here are some examples,
  so you can see how each letter is written in combination with a horizontal and vertical letter."""]
ss22cards = stringsToCards ["나", "노", "더", "두", "너", "누", "다", "도", "니", "디"]

ss23 =  ssTranscriptionGame "Transcription Game!" ss23text ss23cards
ss23text =  ["""This game is a little different from the last one! Now, you have to transcribe each 
  word you hear!"""
            ,"Just for clarity, each vowel should be transcribed as follows: ", "아: a\t어: eo\t오: o\t우: u"
            ,"으: eu\t이: i", "and the consonants (regardless of position in the word) as"
            ,"ㄱ: g\tㄴ: n\tㄷ: d\tㅇ: (nothing)"]

ss23cards = stringsToCards ["가기", "어느", "아기", "이다", "아니다", "나누다", "구두"]

ss24 = ssLesson "The first two consonants: ㄹ and ㅁ" ss24text ss24cards 
ss24text = ["""Continuing on with the consonants consonants, ㄹ and ㅁ!
  ㅁ is exactly like the English letter "m"!""",
  """ㄹ is most similar to the English letters "r" and "l". It makes the "l" sound at the start of words, 
  and the "r" sound elsewhere. """,
  """Here are some examples,
  so you can see how each letter is written in combination with a horizontal and vertical letter."""]
ss24cards = stringsToCards (String.split "" "로리머무마모미")

ss25 = ssLesson "The first two consonants: ㅂ and ㅅ" ss25text ss25cards 
ss25text = ["""Continuing on with the consonants consonants, ㅂ and ㅅ!
  ㅅ is almost exactly like the English letter "s"! However, when the consonant that comes after it is "i", it 
  is pronounced as "sh", so "시" is "shi".""",
  """ㅂ is most similar to the English letters "p" and "b". It makes the "p" sound at the start of words, 
  and the "b" sound elsewhere. """,
  """Here are some examples,
  so you can see how each letter is written in combination with a horizontal and vertical letter."""]
ss25cards = stringsToCards (String.split "" "사소비부서수시")

ss25e = ssTransMatchGame "Hangul-Romanization matching Game!" ss25etext ss25ecards
ss25etext = ["""Time for some more review! This time, match each Hangul word with its romanization!"""]
ss25ecards = stringsToCards ["다시", "바로", "부모", "거리", "누나", "아시아"]

ss26 = ssLesson "The first two consonants: ㅈ and ㅎ" ss26text ss26cards 
ss26text = ["""Continuing on with the consonants consonants, ㅈ and ㅎ!
  ㅎ is exactly like the English letter "h"!""",
  """ㅈ is most similar to the English sounds "ch" and "j" (as in judge). It makes the "ch" sound 
  at the start of words, and the "j" sound elsewhere. """,
  """Here are some examples,
  so you can see how each letter is written in combination with a horizontal and vertical letter."""]
ss26cards = stringsToCards (String.split "" "저조하후자호히지주")



ss27 = ssKorTransGame "Transcription Game - Korean!" ss27text ss27chars ss27cards
ss27chars = Set.fromList (List.append (String.split "" "ㅂㅈㄷㄱㅅㅁㄴㅇㄹㅎㅓㅏㅣㅜㅡㅗ") ["Shift", "Back"])
ss27text =  ["""This game is a little different from the last one! Now, you have to transcribe each word 
  you hear, using Hangul! We've provided a helpful keyboard for you to use, if you don't have a Hangul 
  keyboard already."""]
ss27cards = stringsToCards ["머리", "하루", "스시", "서로", "부자", "자기", "비누", "허리"]


---------------------------------
--Sandbox section
---------------------------------

sectionSandbox : Section 
sectionSandbox =  { name = "Sandbox: Play around!"
      , subsections = [ssSandbox]
      }

ssSandbox =  ssKorTransGame "Type in Hangul!" ssSandboxText allLetters []
ssSandboxText =  ["""Just type whatever you want!"""]