module Cards exposing (..)

import Set exposing (Set, member, isEmpty, insert)
import Html.Styled exposing (Html, fromUnstyled)
import Color exposing (..)
import Collage exposing (..)
import Collage.Events as CE exposing (..)
import Collage.Layout as CL exposing (..)
import Collage.Text as CT exposing (..)
import Collage.Render exposing (..)
import Dict exposing (Dict)
import Types exposing (..)

type CardColor  = White 
                | Green 
                | Red 
                | Blue 

--Makes a card that on clicked performs the given image. 
--the card has the given image, and given text
stringToImage : (String -> Msg) -> String -> String -> Collage Msg  
stringToImage action file k = 
  let 
    korText = fromString k
      |> typeface (Font "Malgun Gothic")
      |> size enormous
      |> weight Bold
      |> CT.color (rgb255 71 69 76)
      |> Collage.rendered
    cardRect = Collage.image (125, 175) file
      |> at base korText 
      |> CE.onClick (action k)
    in --Adding a border
      Collage.rectangle 140 190
      |> filled (uniform white)
      |> at base cardRect

whiteFile = "assets/tile.png"
greenFile = "assets/correctTile.png"
redFile = "assets/incorrectTile.png"
blueFile = "assets/selectedTile.png"

cardToImage : String -> Card -> Collage Msg 
cardToImage s (Card k _ _) = stringToImage (CardClicked) s k

whiteCard : Card -> Collage Msg 
whiteCard = cardToImage "assets/tile.png"

greenCard : Card -> Collage Msg 
greenCard = cardToImage "assets/correctTile.png"

redCard : Card -> Collage Msg 
redCard = cardToImage "assets/incorrectTile.png"

whiteCards : List Card -> List (Collage Msg)
whiteCards = List.map whiteCard

--colors a card green red or white depending on if incorrect correct or neither.
colorCard : Set KoreanName -> Set KoreanName -> Card -> Collage Msg
colorCard correct incorrect card = let (Card name _ _) = card in
    if member name correct 
        then greenCard card 
        else if member name incorrect 
            then redCard card 
            else whiteCard card

colorCards : Set KoreanName -> Set KoreanName -> List Card -> List (Collage Msg)
colorCards correct incorrect = List.map (colorCard correct incorrect)

cardNames : List Card -> List String
cardNames = List.map (\(Card k _ _) -> k)

--Displays card collages in rows of four or fewer
displayHelper : Collage Msg -> List (Collage Msg) -> Collage Msg
displayHelper collage cs = case cs of
  [] -> 
    collage
  a :: b :: c :: d :: rest -> 
    displayHelper (vertical [collage, CL.horizontal [a, b, c, d]]) rest
  lessThanFour -> vertical [collage, CL.horizontal lessThanFour]

--cards for normal use
displayCards : Set KoreanName -> Set KoreanName -> List Card -> Html Msg 
displayCards correct incorrect cards =
    colorCards correct incorrect cards
        |> displayHelper CL.empty
        |> svg 
        |> fromUnstyled

matchImage = stringToImage MatchClick

--One card (for matching game)
colorDoubleSided : Maybe String -> Set String -> (Card, Side) -> Collage Msg 
colorDoubleSided selected correct ((Card h _ r), side) = 
    let 
        k = case side of 
            Hangul -> h
            Romanization -> r 
        sel = case selected of 
            Nothing -> False 
            Just selString -> (k == selString)
    in 
        if sel 
            then matchImage blueFile k
            else if member k correct 
                then matchImage greenFile k 
                else matchImage whiteFile k 

--many cards (for matching game)
displayDoubleSided : Maybe String -> Set String -> List (Card, Side) -> Html Msg
displayDoubleSided selected correct cards =
    List.map (colorDoubleSided selected correct) cards 
        |> displayHelper CL.empty
        |> svg 
        |> fromUnstyled

displayCardsMonocolor : CardColor -> List Card -> Html Msg 
displayCardsMonocolor color cards =
    let 
        cardFunc = case color of 
            Green -> greenCard 
            Red -> redCard 
            _ -> whiteCard
    in List.map cardFunc cards 
        |> displayHelper CL.empty
        |> svg 
        |> fromUnstyled


forwardButton : Html Msg 
forwardButton = let f = Collage.image (158, 79) "assets/forward.png" in 
        horizontal [CE.onClick Backward (rotate (degrees 180) f)
                   , CE.onClick Forward f]
        |> svg 
        |> fromUnstyled

inputButton : Html Msg 
inputButton = 
    let 
        text = fromString "Check"
            |> typeface (Font "Malgun Gothic")
            |> size large
            |> weight Bold
            |> CT.color (rgb255 71 69 76)
            |> Collage.rendered
    in 
        Collage.image (115, 50) "assets/button.png"
        |> at base text 
        |> CE.onClick InputSubmitted 
        |> svg
        |> fromUnstyled


--all of the cards in the game
cardList : List Card 
cardList = 
    [ Card "아" "" "a"
    , Card "어" "" "eo"
    , Card "오" "" "o"
    , Card "우" "" "u"
    , Card "으" "" "eu"
    , Card "이" "" "i" 
    , Card "가" "go" "ga"
    , Card "거" "" "geo"
    , Card "고" "" "go"
    , Card "구" "" "gu"
    , Card "그" "that" "geu"
    , Card "기" "" "gi"
    , Card "다" "all" "da"
    , Card "더" "more" "deo"
    , Card "도" "also" "do"
    , Card "두" "" "du"
    , Card "디" "" "di"
    , Card "나" "I" "na"
    , Card "너" "you" "neo"
    , Card "노" "" "no"
    , Card "누" "" "nu"
    , Card "니" "" "ni"
    , Card "가기" "going" "gagi"
    , Card "어느" "which" "eoneu"
    , Card "아기" "baby" "agi"
    , Card "이다" "to be" "ida"
    , Card "아니다" "to not be" "anida"
    , Card "나누다" "to share" "nanuda"
    , Card "구두" "high heels" "gudu"
    , Card "로" "road" "ro"
    , Card "리" "" "ri" 
    , Card "머" "" "meo"
    , Card "무" "radish" "mu"
    , Card "마" "" "ma" 
    , Card "모" "" "mo"
    , Card "미" "" "mi"
    , Card "사" "four" "sa" 
    , Card "소" "cow" "so" 
    , Card "비" "rain" "bi"
    , Card "부" "" "bu"
    , Card "서" "west" "seo" 
    , Card "수" "" "su"
    , Card "시" "poem" "si"
    , Card "저" "I (polite)" "jeo" 
    , Card "조" "" "jo"
    , Card "하" "" "ha"
    , Card "후" "after" "hu"
    , Card "자" "sleep" "ja" 
    , Card "호" "" "ho" 
    , Card "히" "" "hi"
    , Card "지" "" "ji"
    , Card "주" "week" "ju"
    , Card "머리" "head" "meori"
    , Card "하루" "one day" "haru"
    , Card "스시" "sushi" "seusi"
    , Card "서로" "one another" "seoro"
    , Card "부자" "boss" "buja"
    , Card "자기" "self" "jagi"
    , Card "비누" "soap" "binu"
    , Card "허리" "waist" "heori"
    , Card "다시" "again" "dasi"
    , Card "바로" "directly" "baro"
    , Card "부모" "parents" "bumo"
    , Card "거리" "road" "geori"
    , Card "누나" "older sister" "nuna"
    , Card "아시아" "Asia" "asia"
    ]

cardDict : Dict KoreanName Card 
cardDict = Dict.fromList (List.map (\((Card k _ _ ) as c) -> (k, c)) cardList)

stringsToCards : List String -> List Card 
stringsToCards = List.filterMap (\s -> Dict.get s cardDict)