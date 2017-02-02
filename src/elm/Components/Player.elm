module Components.Player exposing (..)

import Html exposing (Html, ul, p, text, li, a, div, nav)
import Html.Attributes exposing (class)
import Html.Events exposing (onWithOptions)
import Json.Decode as JD

import Components.FontAwesome exposing (fa)
import Models.File exposing (File)

-- MODEL

type WorkingState
  = Playing
  | Paused

type Model
  = Stopped
  | Working WorkingState Int File (List File) (List File)

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

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    Stop ->
      (Stopped, Cmd.none)
    Pause ->
      case model of
        Working Playing time file previous next ->
          (Working Paused time file previous next, Cmd.none)
        _ ->
          (model, Cmd.none)
    Play ->
      case model of
        Working Paused time file previous next ->
          (Working Playing time file previous next, Cmd.none)
        _ ->
          (model, Cmd.none)
    Backward ->
      case model of
        Working state time file previous next ->
          if time > 5 then
            (Working state 0 file previous next, Cmd.none)
          else
            case previous of
              [] ->
                (Working state 0 file previous next, Cmd.none)
              file :: newPrevious ->
                (Working state 0 file newPrevious (file :: next), Cmd.none)
        Stopped ->
          (model, Cmd.none)
    Forward ->
      case model of
        Working state time file previous next ->
          case next of
            [] ->
              (model, Cmd.none)
            file :: newNext ->
              (Working state 0 file (file :: previous) newNext, Cmd.none)
        Stopped ->
          (model, Cmd.none)
    Update files file ->
      let (previous, next) = splitList files file
      in (Working Playing 0 file previous next, Cmd.none)

-- VIEW

view : Model -> List (Html Msg)
view model =
  let
    pad = String.padLeft 2 '0' << toString
    toHumanTime time = pad (time // 60) ++ ":" ++ pad (rem time 60)
    describe txt time = txt ++ " (" ++ toHumanTime time ++ ")"
    description txt time controls =
      ul [ class "nav navbar-nav navbar-left" ]
        (controls ++ [ p [ class "navbar-text" ] [ text (describe txt time) ] ])
    control icon msg disabled_ =
      li (if disabled_ then [ class "disabled" ] else [])
        [ a
          [ onWithOptions "click" { stopPropagation = True, preventDefault = True } (JD.succeed msg) ]
          [ fa icon ]
        ]
    backwardControl previous =
      [control "backward" Backward (List.isEmpty previous)]
    stopControl =
      [control "stop" Stop False]
    playPauseControl state =
      [ case state of
        Playing -> control "pause" Pause False
        Paused -> control "play" Play False
      ]
    forwardControl next =
      [control "forward" Forward (List.isEmpty next)]
  in
    case model of
      Stopped ->
        []
      Working state time { name } previous next ->
        [ nav [ class "navbar navbar-default navbar-fixed-bottom" ]
          [ div [ class "container" ]
            [ description name time
              (backwardControl previous ++ stopControl ++ playPauseControl state ++ forwardControl next)
            ]
          ]
        ]

-- FUNCTIONS

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
