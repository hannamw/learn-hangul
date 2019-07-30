module Navigation exposing (..)

import Types exposing (..)

--------------------------
--- Navigation
--------------------------

--all of these just find sections / subsections and load model for navigation

--given a section name returns the section and following sections
findSection : String -> List Section -> Maybe (Section, List Section)
findSection s secs = case secs of 
  [] -> Nothing 
  sec :: rest -> if s == sec.name 
    then Just (sec, rest)
    else findSection s rest 
--above, but for subsections
findSubsection : String -> List Subsection -> Maybe (Subsection, List Subsection)
findSubsection s ssecs = case ssecs of 
  [] -> Nothing 
  ssec :: rest -> if s == ssec.name 
    then Just (ssec, rest)
    else findSubsection s rest 
--given a list of sections, and then section and subsection names, returns either
--the next subsection and its section or the current (if there are no more subsections)
nextSubsection : List Section -> String -> String -> (Section, Subsection)
nextSubsection secList section subsection =
  let maybeSection = findSection section secList in 
    case maybeSection of 
      Nothing -> Debug.todo "Couldn't find the current section in list of sections!"
      Just (sec, secs) -> let maybeSubsection = findSubsection subsection sec.subsections in 
        case maybeSubsection of 
          Nothing -> Debug.todo "Couldn't find current ss in list of ss!"
          Just (subsec, subsecs) -> case subsecs of 
            nextSS :: _ -> (sec, nextSS)
            [] -> case secs of 
              [] -> (sec, subsec)
              nextS :: _ -> case nextS.subsections of 
                [] -> Debug.todo "Encountered section with no subsections!"
                nextSS :: _ -> (nextS, nextSS)
            
--sends a message to load the next subsection.
forward : List Section -> Model -> Cmd Msg 
forward secList ((M m) as model) = let (section, subsection) = (nextSubsection secList m.section m.subsection) in 
  updateModel section subsection model

prevSubsection : List Section -> String -> String -> (Section, Subsection)
prevSubsection secList section subsection =
  let maybeSection = findSection section (List.reverse secList) in 
    case maybeSection of 
      Nothing -> Debug.todo "Couldn't find the current section in list of sections!"
      Just (sec, secs) -> let maybeSubsection = findSubsection subsection (List.reverse sec.subsections) in 
        case maybeSubsection of 
          Nothing -> Debug.todo "Couldn't find current ss in list of ss!"
          Just (subsec, subsecs) -> case subsecs of 
            nextSS :: _ -> (sec, nextSS)
            [] -> case secs of 
              [] -> (sec, subsec)
              nextS :: _ -> case (List.reverse nextS.subsections) of 
                [] -> Debug.todo "Encountered section with no subsections!"
                nextSS :: _ -> (nextS, nextSS)
            


backward : List Section -> Model -> Cmd Msg 
backward secList ((M m) as model) = let (section, subsection) = (prevSubsection secList m.section m.subsection) in 
  updateModel section subsection model

--loads a subsection by calling its loadFunction on the new model
updateModel :  Section -> Subsection -> Model -> Cmd Msg
updateModel section subsection ((M m) as model) = subsection.loadFunction
  (M {m | section = section.name
      , subsection = subsection.name
      , state = subsection.defaultState
      , view = subsection.view 
      , update = subsection.update})