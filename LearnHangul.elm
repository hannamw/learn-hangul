module LearnHangul exposing (main)

import Browser
import Css exposing (..)
import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes as HA exposing (css, href, src, autoplay, controls, style, rel)
import Html.Styled.Events exposing (onClick)
import Hex 

import Cards exposing (..)
import Types exposing (..)
import Sections exposing (..)

type alias Flags = ()

--The content is mostly in Sections.elm; you'll only find a little here.

main : Program Flags Model Msg
main = 
  Browser.element
    { init = init
    , view = view >> toUnstyled
    , update = update
    , subscriptions = subscriptions
    }

init : Flags -> (Model, Cmd Msg)
init () =
  (M { section = section1.name
    , subsection = ss11.name
    , view = ss11.view
    , update = ss11.update
    , state = Lesson
    , playSound = (True, Nothing)
    }
  , Cmd.none)


--SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model = Sub.batch []

-- UPDATE
update : Msg -> Model -> (Model, Cmd Msg)
update msg ((M m) as model)  =
  m.update msg model

-- VIEW
--wraps the view function given by the subsection, adding the sound and nav buttons
view : Model -> Html Msg
view ((M m) as model) =
  let 
    normalView =  div [] 
                  [ m.view model
                  , div [forwardStyle] [forwardButton]
                  ]
    contents = addSound model normalView
                |> addTypefaces
  in div [] contents
  

--the sound...of silence
silentSound : Html Msg  
silentSound = audio [ src "assets/silent.mp3", autoplay False, controls False] []

--a sound that autoplays
autoplaySound : String -> Html Msg 
autoplaySound s = audio [src s, autoplay True, controls False] []

--if the model has a sound, adds it to the model. otherwise, adds the sound of silence
addSound : Model -> Html Msg -> List (Html Msg)
addSound (M m) h = case m.playSound of 
  (_, Nothing) -> [silentSound, h]
  (b, Just sound) -> let autoAudio = autoplaySound sound in
    if b  
      then (wrapper [autoAudio]) :: [h]
      else [autoAudio, h]

--adds the typeface(s) used to the model
addTypefaces : List (Html Msg) -> List (Html Msg)
addTypefaces h = 
  (node "link" [ href "https://fonts.googleapis.com/css?family=Nanum+Gothic", rel "stylesheet" ] []) :: h