port module Components.Player exposing (..)

import Html exposing (Html, ul, p, text, li, a, div, nav, span)
import Html.Attributes exposing (class, accesskey, href, attribute, style)
import Components.Html exposing (data, aria)
import Html.Events exposing (onWithOptions)
import Json.Decode as JD
import Json.Encode as JE
import Components.FontAwesome exposing (fa)
import Data.File exposing (File)


-- MODEL


type WorkingState
    = Playing
    | Paused


type Model
    = Stopped
    | Working WorkingState ( Float, Float ) File (List File) (List File)


initialModel : Model
initialModel =
    Stopped



-- UPDATE


type Msg
    = Stop
    | Pause
    | Play
    | Backward
    | Forward
    | Update (List File) File
    | Append (List File)
    | Sync JD.Value


port webAudioControl : JE.Value -> Cmd msg


syncWebAudio : Model -> String -> Cmd Msg
syncWebAudio player server =
    let
        toJvalue player server =
            case player of
                Working state ( time, _ ) file _ next ->
                    case state of
                        Playing ->
                            let
                                default =
                                    [ ( "action", JE.string "play" ), ( "time", JE.float time ) ] ++ fileTuples file server

                                tail =
                                    case List.head next of
                                        Just nextFile ->
                                            [ ( "next", JE.object (fileTuples nextFile server) ) ]

                                        Nothing ->
                                            []
                            in
                                JE.object (default ++ tail)

                        Paused ->
                            JE.object ([ ( "action", JE.string "pause" ), ( "time", JE.float time ) ] ++ fileTuples file server)

                Stopped ->
                    JE.object [ ( "action", JE.string "stop" ) ]
    in
        webAudioControl (toJvalue player server)


commandNature : Model -> String -> ( Model, Cmd Msg )
commandNature model server =
    ( model, syncWebAudio model server )


update : Msg -> Model -> String -> ( Model, Cmd Msg )
update msg model server =
    case msg of
        Stop ->
            commandNature Stopped server

        Pause ->
            case model of
                Working Playing time file previous next ->
                    commandNature (Working Paused time file previous next) server

                _ ->
                    ( model, Cmd.none )

        Play ->
            case model of
                Working Paused time file previous next ->
                    commandNature (Working Playing time file previous next) server

                _ ->
                    ( model, Cmd.none )

        Backward ->
            case model of
                Working state ( time, dur ) file previous next ->
                    if time > 5 then
                        commandNature (Working Playing ( 0, 1 ) file previous next) server
                    else
                        case previous of
                            [] ->
                                commandNature (Working Playing ( 0, 1 ) file previous next) server

                            file_ :: previous_ ->
                                commandNature (Working Playing ( 0, dur ) file_ previous_ (file :: next)) server

                Stopped ->
                    ( model, Cmd.none )

        Forward ->
            case model of
                Working state time file previous next ->
                    case next of
                        [] ->
                            ( model, Cmd.none )

                        nextFile :: newNext ->
                            commandNature (Working Playing ( 0, 1 ) nextFile (file :: previous) newNext) server

                Stopped ->
                    ( model, Cmd.none )

        Update files file ->
            let
                ( previous, next ) =
                    splitList files file
            in
                commandNature (Working Playing ( 0, 1 ) file previous next) server

        Append newFiles ->
            case model of
                Working state time file previous next ->
                    ( Working state time file previous (next ++ newFiles), Cmd.none )

                Stopped ->
                    case newFiles of
                        file_ :: next_ ->
                            commandNature (Working Playing ( 0, 1 ) file_ [] next_) server

                        _ ->
                            ( model, Cmd.none )

        Sync jvalue ->
            let
                time =
                    Result.withDefault 0 (JD.decodeValue (JD.field "offset" JD.float) jvalue)

                dur =
                    Result.withDefault 1 (JD.decodeValue (JD.field "progress" JD.float) jvalue)
            in
                case JD.decodeValue (JD.field "state" JD.string) jvalue of
                    Ok "paused" ->
                        case model of
                            Working _ _ file previous next ->
                                ( Working Paused ( time, dur ) file previous next, Cmd.none )

                            Stopped ->
                                ( Stopped, Cmd.none )

                    Ok "playing" ->
                        case model of
                            Working _ _ file previous next ->
                                case JD.decodeValue (JD.field "remaining" JD.float) jvalue of
                                    Ok remaining ->
                                        if remaining <= 5 then
                                            let
                                                cmd =
                                                    case List.head next of
                                                        Just nextFile ->
                                                            (webAudioControl << JE.object << (::) ( "action", JE.string "preload" )) (fileTuples nextFile server)

                                                        Nothing ->
                                                            Cmd.none
                                            in
                                                ( Working Playing ( time, dur ) file previous next, cmd )
                                        else
                                            ( Working Playing ( time, dur ) file previous next, Cmd.none )

                                    Err _ ->
                                        ( Working Playing ( time, dur ) file previous next, Cmd.none )

                            Stopped ->
                                ( model, Cmd.none )

                    Ok "finished" ->
                        case model of
                            Working _ _ file previous next ->
                                case next of
                                    file_ :: next_ ->
                                        commandNature (Working Playing ( 0, 1 ) file_ (file :: previous) next_) server

                                    _ ->
                                        ( Stopped, Cmd.none )

                            Stopped ->
                                ( Stopped, Cmd.none )

                    _ ->
                        ( model, Cmd.none )



-- SUB


port webAudioFeedback : (JD.Value -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions _ =
    webAudioFeedback Sync



-- VIEW


view : Model -> List (Html Msg)
view model =
    let
        pad =
            String.padLeft 2 '0' << toString

        toHumanTime secs =
            pad (secs // 60) ++ ":" ++ pad (rem secs 60)

        describe txt time =
            txt ++ " (" ++ (toHumanTime << floor) time ++ ")"

        description txt ( time, dur ) controls =
            ul [ class "nav navbar-nav navbar-left" ]
                (controls
                    ++ [ p [ class "navbar-text" ]
                            [ text (describe txt time)
                            , div [ class "progress progress-player" ]
                                [ div
                                    ([ class "progress-bar progress-bar-info"
                                     , style [ ( "width", toString (time / dur * 100) ++ "%" ) ]
                                     , attribute "role" "progressbar"
                                     ]
                                        ++ aria
                                            [ ( "valuenow", toString time )
                                            , ( "valuemin", "0" )
                                            , ( "valuemax", toString dur )
                                            ]
                                    )
                                    []
                                ]
                            ]
                       ]
                )

        control icon msg key disabled_ =
            li
                (if disabled_ then
                    [ class "disabled" ]
                 else
                    []
                )
                [ a
                    [ onWithOptions "click" { stopPropagation = True, preventDefault = True } (JD.succeed msg)
                    , accesskey key
                    ]
                    [ fa icon ]
                ]

        backwardControl previous =
            control "backward" Backward 'z' (List.isEmpty previous)

        stopControl =
            control "stop" Stop 'x' False

        playPauseControl state =
            case state of
                Playing ->
                    control "pause" Pause 'c' False

                Paused ->
                    control "play" Play 'c' False

        forwardControl next =
            control "forward" Forward 'v' (List.isEmpty next)
    in
        case model of
            Stopped ->
                []

            Working state time file previous next ->
                let
                    allFiles =
                        List.foldl (::) (file :: next) previous

                    playlistRow file =
                        li []
                            [ a
                                [ onWithOptions "click"
                                    { stopPropagation = True
                                    , preventDefault = True
                                    }
                                    (JD.succeed (Update allFiles file))
                                ]
                                [ text file.name ]
                            ]

                    history =
                        case previous of
                            [] ->
                                []

                            val ->
                                [ li [ class "dropdown-header" ] [ text "history" ] ]
                                    ++ (List.map playlistRow << List.reverse << List.take 2) val

                    current =
                        [ li [ class "dropdown-header" ] [ text "now playing" ]
                        , li [ class "disabled" ] [ a [] [ text file.name ] ]
                        ]

                    upcoming =
                        case next of
                            [] ->
                                []

                            val ->
                                [ li [ class "dropdown-header" ] [ text "up next" ] ]
                                    ++ List.map playlistRow val

                    playlist =
                        history ++ current ++ upcoming
                in
                    [ nav [ class "navbar navbar-default navbar-fixed-bottom" ]
                        [ div [ class "container" ]
                            [ description file.name
                                time
                                [ backwardControl previous
                                , stopControl
                                , playPauseControl state
                                , forwardControl next
                                ]
                            , ul [ class "nav navbar-nav navbar-right" ]
                                [ li [ class "dropdown" ]
                                    [ a
                                        ([ href "#"
                                         , class "dropdown-toggle"
                                         , attribute "role" "button"
                                         ]
                                            ++ data
                                                [ ( "toggle", "dropdown" )
                                                ]
                                            ++ aria
                                                [ ( "haspopup", "true" )
                                                , ( "expanded", "false" )
                                                ]
                                        )
                                        [ fa "list" ]
                                    , ul [ class "dropdown-menu dropdown-scrollable" ] playlist
                                    ]
                                ]
                            ]
                        ]
                    ]



-- FUNCTIONS


fileUrl : String -> String -> String
fileUrl server path =
    server ++ "/files/" ++ path


fileTuples : File -> String -> List ( String, JE.Value )
fileTuples file server =
    [ ( "url", JE.string (fileUrl server file.path) )
    , ( "id", JE.int file.id )
    ]


splitList : List a -> a -> ( List a, List a )
splitList list separator =
    let
        splitList_ acc list separator =
            case list of
                [] ->
                    ( acc, [] )

                hd :: tl ->
                    if hd == separator then
                        ( acc, tl )
                    else
                        splitList_ (hd :: acc) tl separator
    in
        splitList_ [] list separator
