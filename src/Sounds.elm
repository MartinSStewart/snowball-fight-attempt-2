module Sounds exposing (Sounds, loadingFinished, requestSounds, soundUrls)

import Audio exposing (AudioCmd)
import SeqDict exposing (SeqDict)


type alias Sounds =
    { blip : Audio.Source
    , charge : Audio.Source
    , charge2 : Audio.Source
    , crackle : Audio.Source
    , dead1 : Audio.Source
    , dead2 : Audio.Source
    , dead3 : Audio.Source
    , dead4 : Audio.Source
    , dead5 : Audio.Source
    , erase : Audio.Source
    , error : Audio.Source
    , footstep1 : Audio.Source
    , footstep2 : Audio.Source
    , footstep3 : Audio.Source
    , footstep4 : Audio.Source
    , footstep5 : Audio.Source
    , footstep6 : Audio.Source
    , footstep7 : Audio.Source
    , footstep8 : Audio.Source
    , lightSwitch : Audio.Source
    , meow : Audio.Source
    , pageTurn : Audio.Source
    , pop : Audio.Source
    , railToggle : Audio.Source
    , throw1 : Audio.Source
    , throw2 : Audio.Source
    , throw3 : Audio.Source
    , throw4 : Audio.Source
    , throw5 : Audio.Source
    , throw6 : Audio.Source
    , throw7 : Audio.Source
    , throw8 : Audio.Source
    , throw9 : Audio.Source
    , throw10 : Audio.Source
    , throw11 : Audio.Source
    , throw12 : Audio.Source
    , throw13 : Audio.Source
    , throw14 : Audio.Source
    , whoosh : Audio.Source
    }


soundUrls : List String
soundUrls =
    [ "/blip.mp3"
    , "/charge.mp3"
    , "/charge2.mp3"
    , "/crackle.mp3"
    , "/dead1.mp3"
    , "/dead2.mp3"
    , "/dead3.mp3"
    , "/dead4.mp3"
    , "/dead5.mp3"
    , "/erase.mp3"
    , "/error.mp3"
    , "/footstep1.mp3"
    , "/footstep2.mp3"
    , "/footstep3.mp3"
    , "/footstep4.mp3"
    , "/footstep5.mp3"
    , "/footstep6.mp3"
    , "/footstep7.mp3"
    , "/footstep8.mp3"
    , "/light-switch.mp3"
    , "/meow.mp3"
    , "/page-turn.mp3"
    , "/pop.mp3"
    , "/rail-toggle.mp3"
    , "/throw1.ogg"
    , "/throw2.ogg"
    , "/throw3.ogg"
    , "/throw4.ogg"
    , "/throw5.ogg"
    , "/throw6.ogg"
    , "/throw7.ogg"
    , "/throw8.ogg"
    , "/throw9.ogg"
    , "/throw10.ogg"
    , "/throw11.ogg"
    , "/throw12.ogg"
    , "/throw13.ogg"
    , "/throw14.ogg"
    , "/whoosh.mp3"
    ]


requestSounds : (String -> Result Audio.LoadError Audio.Source -> msg) -> AudioCmd msg
requestSounds loadedSound =
    List.map (\url -> Audio.loadAudio (loadedSound url) url) soundUrls |> Audio.cmdBatch


loadingFinished : SeqDict String (Result Audio.LoadError Audio.Source) -> Maybe Sounds
loadingFinished sounds =
    let
        loadSound : ( List String, Maybe (Audio.Source -> b) ) -> ( List String, Maybe b )
        loadSound ( urlsLeft, soundsFinished ) =
            case ( urlsLeft, soundsFinished ) of
                ( head :: rest, Just soundsFinished_ ) ->
                    case SeqDict.get head sounds of
                        Just (Ok source) ->
                            ( rest, soundsFinished_ source |> Just )

                        _ ->
                            ( [], Nothing )

                _ ->
                    ( [], Nothing )
    in
    ( soundUrls, Just Sounds )
        |> loadSound
        |> loadSound
        |> loadSound
        |> loadSound
        |> loadSound
        |> loadSound
        |> loadSound
        |> loadSound
        |> loadSound
        |> loadSound
        |> loadSound
        |> loadSound
        |> loadSound
        |> loadSound
        |> loadSound
        |> loadSound
        |> loadSound
        |> loadSound
        |> loadSound
        |> loadSound
        |> loadSound
        |> loadSound
        |> loadSound
        |> loadSound
        |> loadSound
        |> loadSound
        |> loadSound
        |> loadSound
        |> loadSound
        |> loadSound
        |> loadSound
        |> loadSound
        |> loadSound
        |> loadSound
        |> loadSound
        |> loadSound
        |> loadSound
        |> loadSound
        |> loadSound
        |> Tuple.second
