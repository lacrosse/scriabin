module Components.Player exposing (..)

import Array exposing (Array)
import Models.File exposing (File)

-- MODEL

type Current
  = Stopped (Maybe File)
  | Paused File Int
  | Playing File Int

type alias Model =
  { previous : Array File
  , current : Current
  , next : Array File
  }

initialModel : Model
initialModel =
  { previous = Array.empty
  , current = Stopped Nothing
  , next = Array.empty
  }

-- UPDATE

type Msg
  = Stop
  | Pause
  | Play

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    Stop ->
      case model.current of
        Playing file _ ->
          ( { model | current = Stopped (Just file) }, Cmd.none )
        Paused file _ ->
          ( { model | current = Stopped (Just file) }, Cmd.none )
        _ ->
          ( model, Cmd.none )
    Pause ->
      case model.current of
        Playing file time ->
          ( { model | current = Paused file time }, Cmd.none )
        _ ->
          ( model, Cmd.none )
    Play ->
      case model.current of
        Stopped maybeFile ->
          case maybeFile of
            Just file ->
              ( { model | current = Playing file 0 }, Cmd.none )
            Nothing ->
              ( model, Cmd.none )
        Paused file time ->
          ( { model | current = Playing file time }, Cmd.none )
        _ ->
          ( model, Cmd.none )
