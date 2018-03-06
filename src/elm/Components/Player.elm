port module Components.Player exposing (..)

import Html exposing (Html, ul, p, text, li, a, div, nav, span)
import Html.Attributes exposing (class, accesskey, href, attribute, style)
import Components.Html exposing (data, aria)
import Components.PageTitle
import Components.FontAwesome exposing (fa)
import Html.Events exposing (onWithOptions)
import Events exposing (onMouseUpRelativeWidth)
import Json.Decode as JD
import Json.Encode as JE
import Data.Assemblage exposing (Assemblage)
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
    | Seek Float
    | Backward
    | Forward
    | Update Assemblage
    | UpdateSplitFiles (List File) File
    | Append (List File)
    | Sync JD.Value


port webAudioControl : JE.Value -> Cmd msg


toWebAudioCommand : String -> Model -> JE.Value
toWebAudioCommand endpoint player =
    case player of
        Working state ( time, _ ) file _ _ ->
            let
                action =
                    case state of
                        Playing ->
                            "play"

                        Paused ->
                            "pause"
            in
                JE.object (( "action", JE.string action ) :: ( "time", JE.float time ) :: Data.File.toJsonList endpoint file)

        Stopped ->
            JE.object [ ( "action", JE.string "stop" ) ]


syncWebAudio : String -> Model -> Cmd Msg
syncWebAudio endpoint player =
    (webAudioControl << toWebAudioCommand endpoint) player


toTitleCmd : Model -> Cmd a
toTitleCmd model =
    case model of
        Working state _ current _ _ ->
            case state of
                Playing ->
                    Components.PageTitle.set ("▶️ " ++ current.name)

                Paused ->
                    Components.PageTitle.set ("⏸ " ++ current.name)

        Stopped ->
            Components.PageTitle.reset


commandNature : Model -> String -> ( Model, Cmd Msg )
commandNature model endpoint =
    let
        titleCmd =
            toTitleCmd model

        webAudioCmd =
            syncWebAudio endpoint model
    in
        ( model, Cmd.batch ([ titleCmd, webAudioCmd ]) )


update : Msg -> Model -> String -> ( Model, Cmd Msg )
update msg model endpoint =
    case msg of
        Stop ->
            commandNature Stopped endpoint

        Pause ->
            case model of
                Working Playing time file previous next ->
                    commandNature (Working Paused time file previous next) endpoint

                _ ->
                    ( model, Cmd.none )

        Play ->
            case model of
                Working Paused time file previous next ->
                    commandNature (Working Playing time file previous next) endpoint

                _ ->
                    ( model, Cmd.none )

        Seek newProgressRelative ->
            case model of
                Working state ( _, dur ) file previous next ->
                    let
                        newTime =
                            dur * newProgressRelative
                    in
                        commandNature (Working state ( newTime, dur ) file previous next) endpoint

                Stopped ->
                    ( model, Cmd.none )

        Backward ->
            case model of
                Working state ( time, dur ) file previous next ->
                    if time > 5 then
                        commandNature (Working Playing ( 0, 1 ) file previous next) endpoint
                    else
                        case previous of
                            [] ->
                                commandNature (Working Playing ( 0, 1 ) file previous next) endpoint

                            file_ :: previous_ ->
                                commandNature (Working Playing ( 0, dur ) file_ previous_ (file :: next)) endpoint

                Stopped ->
                    ( model, Cmd.none )

        Forward ->
            case model of
                Working state time file previous next ->
                    case next of
                        [] ->
                            ( model, Cmd.none )

                        nextFile :: newNext ->
                            commandNature (Working Playing ( 0, 1 ) nextFile (file :: previous) newNext) endpoint

                Stopped ->
                    ( model, Cmd.none )

        Update assemblage ->
            case Data.Assemblage.childrenFiles assemblage of
                [] ->
                    ( model, Cmd.none )

                file :: next ->
                    commandNature (Working Playing ( 0, 1 ) file [] next) endpoint

        UpdateSplitFiles files file ->
            let
                ( previous, next ) =
                    splitList files file
            in
                commandNature (Working Playing ( 0, 1 ) file previous next) endpoint

        Append newFiles ->
            case model of
                Working state time file previous next ->
                    ( Working state time file previous (next ++ newFiles), Cmd.none )

                Stopped ->
                    case newFiles of
                        file_ :: next_ ->
                            commandNature (Working Playing ( 0, 1 ) file_ [] next_) endpoint

                        _ ->
                            ( model, Cmd.none )

        Sync jvalue ->
            let
                time =
                    (Result.withDefault 0 << JD.decodeValue (JD.field "offset" JD.float)) jvalue

                dur =
                    (Result.withDefault 1 << JD.decodeValue (JD.field "progress" JD.float)) jvalue
            in
                case jvalue |> JD.decodeValue (JD.field "state" JD.string) of
                    Ok "paused" ->
                        case model of
                            Working _ _ file previous next ->
                                ( Working Paused ( time, dur ) file previous next, Cmd.none )

                            Stopped ->
                                ( Stopped, Cmd.none )

                    Ok "playing" ->
                        case model of
                            Working _ _ file previous next ->
                                ( Working Playing ( time, dur ) file previous next, Cmd.none )

                            Stopped ->
                                ( model, Cmd.none )

                    Ok "finished" ->
                        case model of
                            Working _ _ file previous next ->
                                case next of
                                    file_ :: next_ ->
                                        commandNature (Working Playing ( 0, 1 ) file_ (file :: previous) next_) endpoint

                                    _ ->
                                        ( Stopped, Cmd.none )

                            Stopped ->
                                ( Stopped, Cmd.none )

                    Ok "stopped" ->
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
    case model of
        Stopped ->
            []

        Working state time file previous next ->
            let
                pad =
                    String.padLeft 2 '0' << toString

                toHumanTime secs =
                    pad (secs // 60) ++ ":" ++ pad (rem secs 60)

                describe txt time =
                    txt ++ " (" ++ (toHumanTime << floor) time ++ ")"

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
                    let
                        ( icon, msg ) =
                            case state of
                                Playing ->
                                    ( "pause", Pause )

                                Paused ->
                                    ( "play", Play )
                    in
                        control icon msg 'c' False

                forwardControl next =
                    control "forward" Forward 'v' (List.isEmpty next)

                description txt ( time, dur ) state previous next =
                    ul [ class "nav navbar-nav navbar-left" ]
                        ([ backwardControl previous
                         , stopControl
                         , playPauseControl state
                         , forwardControl next
                         ]
                            ++ [ p [ class "navbar-text" ]
                                    [ text (describe txt time)
                                    , div [ class "progress progress-player", onMouseUpRelativeWidth Seek ]
                                        [ div
                                            ([ class "progress-bar progress-bar-info progress-bar-player"
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

                allFiles =
                    List.foldl (::) (file :: next) previous

                playlistRow file =
                    li []
                        [ a
                            [ onWithOptions "click"
                                { stopPropagation = True
                                , preventDefault = True
                                }
                                (JD.succeed (UpdateSplitFiles allFiles file))
                            ]
                            [ text file.name ]
                        ]

                history =
                    (List.map playlistRow << List.reverse << List.take 2) previous

                current =
                    [ li [ class "disabled" ]
                        [ a []
                            [ span [ class "la-line-scale-party la-dark la-sm", style [ ( "display", "inline" ) ] ]
                                (List.repeat 5 (div [] []))
                            , text (" " ++ file.name)
                            ]
                        ]
                    ]

                upcoming =
                    List.map playlistRow next

                playlist =
                    history ++ current ++ upcoming
            in
                [ nav [ class "navbar navbar-default navbar-fixed-bottom" ]
                    [ div [ class "container" ]
                        [ description file.name time state previous next
                        , ul [ class "nav navbar-nav navbar-right" ]
                            [ li [ class "dropdown" ]
                                [ a
                                    ([ href "#"
                                     , class "dropdown-toggle"
                                     , attribute "role" "button"
                                     ]
                                        ++ data [ ( "toggle", "dropdown" ) ]
                                        ++ aria [ ( "haspopup", "true" ), ( "expanded", "false" ) ]
                                    )
                                    [ fa "list" ]
                                , ul [ class "dropdown-menu dropdown-scrollable" ] playlist
                                ]
                            ]
                        ]
                    ]
                ]



-- FUNCTIONS


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
