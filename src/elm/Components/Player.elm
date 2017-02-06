port module Components.Player exposing (..)

import Html exposing (Html, ul, p, text, li, a, div, nav)
import Html.Attributes exposing (class, accesskey)
import Html.Events exposing (onWithOptions)
import Json.Decode as JD
import Json.Encode as JE

import Components.FontAwesome exposing (fa)
import Models.File exposing (File)

-- MODEL

type WorkingState
  = Playing
  | Paused

type Model
  = Stopped
  | Working WorkingState Float File (List File) (List File)

initialModel : Model
initialModel = Stopped

-- UPDATE

type Msg
  = Stop
  | Pause
  | Play
  | Backward
  | Forward
  | Update (List File) File
  | Sync JD.Value

port webAudioControl : JE.Value -> Cmd msg

syncWebAudio : Model -> ( String, String ) -> Cmd Msg
syncWebAudio player serverTuple =
  let
    toJvalue player serverTuple =
      case player of
        Working state time file _ next ->
          case state of
            Playing ->
              let
                default =
                  [("action", JE.string "play"), ("time", JE.float time)] ++ fileTuples file serverTuple
                tail =
                  case List.head next of
                    Just nextFile ->
                      [("next", JE.object (fileTuples nextFile serverTuple))]
                    Nothing ->
                      []
              in JE.object (default ++ tail)
            Paused ->
              JE.object ([("action", JE.string "pause"), ("time", JE.float time)] ++ fileTuples file serverTuple)
        Stopped ->
          JE.object [("action", JE.string "stop")]
  in webAudioControl (toJvalue player serverTuple)

commandNature : Model -> ( String, String ) -> ( Model, Cmd Msg )
commandNature model serverTuple =
  (model, syncWebAudio model serverTuple)

update : Msg -> Model -> ( String, String ) -> ( Model, Cmd Msg )
update msg model (server, token) =
  case msg of
    Stop ->
      commandNature Stopped (server, token)
    Pause ->
      case model of
        Working Playing time file previous next ->
          commandNature (Working Paused time file previous next) (server, token)
        _ ->
          commandNature model (server, token)
    Play ->
      case model of
        Working Paused time file previous next ->
          commandNature (Working Playing time file previous next) (server, token)
        _ ->
          commandNature model (server, token)
    Backward ->
      case model of
        Working state time file previous next ->
          if time > 5 then
            commandNature (Working state 0 file previous next) (server, token)
          else
            case previous of
              [] ->
                commandNature (Working state 0 file previous next) (server, token)
              previousFile :: newPrevious ->
                commandNature (Working state 0 previousFile newPrevious (file :: next)) (server, token)
        Stopped ->
          commandNature model (server, token)
    Forward ->
      case model of
        Working state time file previous next ->
          case next of
            [] ->
              commandNature model (server, token)
            nextFile :: newNext ->
              commandNature (Working state 0 nextFile (file :: previous) newNext) (server, token)
        Stopped ->
          commandNature model (server, token)
    Update files file ->
      let (previous, next) = splitList files file
      in commandNature (Working Playing 0 file previous next) (server, token)
    Sync jvalue ->
      let
        time =
          case JD.decodeValue (JD.field "offset" JD.float) jvalue of
            Ok val -> val
            Err err -> 0
      in
        case JD.decodeValue (JD.field "state" JD.string) jvalue of
          Ok "paused" ->
            case model of
              Working _ _ file previous next ->
                (Working Paused time file previous next, Cmd.none)
              Stopped ->
                (Stopped, Cmd.none)
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
                              let
                                jvalue =
                                  JE.object (("action", JE.string "preload") :: fileTuples nextFile (server, token))
                              in webAudioControl jvalue
                            Nothing ->
                              Cmd.none
                      in (Working Playing time file previous next, cmd)
                    else
                      (Working Playing time file previous next, Cmd.none)
                  Err _ ->
                    (Working Playing time file previous next, Cmd.none)
              Stopped ->
                (model, Cmd.none)
          Ok "finished" ->
            case model of
              Working _ _ file previous next ->
                case next of
                  nextFile :: newNext ->
                    commandNature (Working Playing 0 nextFile (file :: previous) newNext) (server, token)
                  _ ->
                    (Stopped, Cmd.none)
              Stopped ->
                (Stopped, Cmd.none)
          _ ->
            (model, Cmd.none)

-- SUB

port webAudio : (JD.Value -> msg) -> Sub msg

subscriptions : Model -> Sub Msg
subscriptions _ = webAudio Sync

-- VIEW

view : Model -> List (Html Msg)
view model =
  let
    pad = String.padLeft 2 '0' << toString
    toHumanTime secs = pad (secs // 60) ++ ":" ++ pad (rem secs 60)
    describe txt time = txt ++ " (" ++ (toHumanTime << floor) time ++ ")"
    description txt time controls =
      ul [ class "nav navbar-nav navbar-left" ]
        (controls ++ [ p [ class "navbar-text" ] [ text (describe txt time) ] ])
    control icon msg key disabled_ =
      li (if disabled_ then [ class "disabled" ] else [])
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
        Playing -> control "pause" Pause 'c' False
        Paused -> control "play" Play 'c' False
    forwardControl next =
      control "forward" Forward 'v' (List.isEmpty next)
  in
    case model of
      Stopped ->
        []
      Working state time { name } previous next ->
        [ nav [ class "navbar navbar-default navbar-fixed-bottom" ]
          [ div [ class "container" ]
            [ description name time
              [ backwardControl previous
              , stopControl
              , playPauseControl state
              , forwardControl next
              ]
            ]
          ]
        ]

-- FUNCTIONS

fileUrl : String -> String -> String
fileUrl server sha256 = server ++ "/files/" ++ sha256

fileTuples : File -> ( String, String ) -> List ( String, JE.Value )
fileTuples file (server, token) =
  [ ("url", JE.string (fileUrl server file.sha256))
  , ("id", JE.int file.id)
  , ("token", JE.string token)
  ]

splitList : List a -> a -> (List a, List a)
splitList list separator =
  case list of
    [] ->
      ([], [])
    hd :: tl ->
      if hd == separator then
        ([], tl)
      else
        let (left, right) = splitList tl separator
        in (hd :: left, right)
