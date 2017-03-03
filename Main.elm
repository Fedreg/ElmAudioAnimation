port module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import String exposing (..)
import Update.Extra.Infix exposing ((:>))
import Dict exposing (..)
import List.Extra exposing (getAt)


main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



--MODEL


type alias Model =
    { noteListA : List Note
    , noteListB : List Note
    , noteListC : List Note
    , noteListD : List Note
    , bpm : Int
    }


type alias Note =
    { hz : Float
    , duration : Float
    , octave : Int
    }


type alias PlayBundle =
    { note : Note
    , tempo : Float
    , waveType : String
    , addDelay : Bool
    }


model =
    { noteListA = [ "ah2", "ch4", "eh5", "gh2" ]
    , noteListB = [ "ah2", "ch4", "fh5", "eh2" ]
    , noteListC = [ "bh2", "eh4", "gh5", "dh2" ]
    , noteListD = [ "ah2", "dh4", "fh5", "ch3" ]
    , bpm = 80
    }


init =
    ( model, Cmd.none )



--UPDATE


port send : PlayBundle -> Cmd msg


type Msg
    = SendNotes (List String)
    | Animate


update msg model =
    case msg of
        SendNotes noteList ->
            let
                note1 =
                    noteSorter (Maybe.withDefault "aq2" <| getAt 0 noteList)

                note2 =
                    noteSorter (Maybe.withDefault "aq2" <| getAt 1 noteList)

                note3 =
                    noteSorter (Maybe.withDefault "aq2" <| getAt 2 noteList)

                note4 =
                    noteSorter (Maybe.withDefault "aq2" <| getAt 3 noteList)
            in
                ( model
                  --, send (PlayBundle (getAt model.index model.notesToSend) (tempo model.bpm) model.waveType model.addDelay))
                , Cmd.batch
                    [ send (PlayBundle note1 (tempo model.bpm) "square" True)
                    , send (PlayBundle note2 (tempo model.bpm) "square" True)
                    , send (PlayBundle note3 (tempo model.bpm) "square" True)
                    , send (PlayBundle note4 (tempo model.bpm) "square" True)
                    ]
                )

        Animate ->
            ( model, Cmd.none )



--subscriptions =
-- subscriptions : Model -> Sub Msg


subscriptions model =
    Sub.none


noteSorter : String -> Note
noteSorter string =
    let
        _ =
            Debug.log "Note_" string
    in
        case (String.length string) of
            3 ->
                Note (frequencies <| slice 0 1 string) (sustain <| slice 1 2 string) (octave <| Result.withDefault 0 <| toInt <| slice 2 3 string)

            4 ->
                if slice 1 2 string == "#" then
                    Note (frequencies <| slice 0 2 string) (sustain <| slice 2 3 string) (octave <| Result.withDefault 0 <| toInt <| slice 3 4 string)
                else
                    Note (frequencies <| slice 0 1 string) (sustain <| slice 1 3 string) (octave <| Result.withDefault 0 <| toInt <| slice 3 4 string)

            5 ->
                Note (frequencies <| slice 0 2 string) (sustain <| slice 2 4 string) (octave <| Result.withDefault 0 <| toInt <| slice 4 5 string)

            _ ->
                Note 0.0 0.0 0


sustain : String -> Float
sustain duration =
    case duration of
        "w" ->
            4.0

        "h" ->
            2.0

        "q" ->
            1.0

        "e" ->
            0.5

        "s" ->
            0.25

        "w." ->
            6.0

        "h." ->
            3.0

        "q." ->
            1.5

        "e." ->
            0.75

        "s." ->
            0.375

        _ ->
            0.0


octave : Int -> Int
octave num =
    case num of
        1 ->
            1

        _ ->
            2 ^ (num - 1)


frequencies : String -> Float
frequencies note =
    case note of
        "c" ->
            130.81

        "c#" ->
            139.0

        "d" ->
            146.83

        "d#" ->
            156.0

        "e" ->
            164.81

        "f" ->
            174.61

        "f#" ->
            185.0

        "g" ->
            196.0

        "g#" ->
            208.0

        "a" ->
            220.0

        "a#" ->
            233.0

        "b" ->
            246.94

        "r" ->
            0.0

        _ ->
            0.0



--Formula for determining frequencies in hz
--110 * (1.059463..)^n


tempo : Int -> Float
tempo bpm =
    (Basics.toFloat 60 / Basics.toFloat bpm) * 0.5



-- VIEW
-- view : Model -> Html Msg


view model =
    div [ style [ ( "textAlign", "center" ), ( "color", "#555" ) ] ]
        [ h1 [ style [ ( "textDecoration", "underline" ), ( "margin", "150px auto 50px" ) ] ] [ text "Elm Audio Animation" ]
        , button [ onClick <| SendNotes model.noteListA, myStyles ] [ text "Play Notes A" ]
        , button [ onClick <| SendNotes model.noteListB, myStyles ] [ text "Play Notes B" ]
        , button [ onClick <| SendNotes model.noteListC, myStyles ] [ text "Play Notes C" ]
        , button [ onClick <| SendNotes model.noteListD, myStyles ] [ text "Play Notes D" ]
        ]


myStyles =
    style
        [ ( "backgroundColor", "#111" )
        , ( "color", "#4d4dff" )
        , ( "border", "1px solid #4d4dff" )
        , ( "margin", " 1rem 20px" )
        ]
