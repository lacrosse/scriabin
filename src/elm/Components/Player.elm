module Components.Player exposing (..)

import Models.File exposing (File)

-- MODEL

type Current
  = Stopped (Maybe File)
  | Paused File Int
  | Playing File Int

type alias Model =
  { previous : List File
  , current : Current
  , next : List File
  }

initialModel : Model
initialModel =
  { previous = []
  , current = Stopped Nothing
  , next = []
  }

-- UPDATE

type Msg
  = Stop
  | Pause
  | Play
  | Backward

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
    Backward ->
      case model.current of
        Stopped maybeFile ->
          case maybeFile of
            Just file ->
              let
                newNext = file :: model.next
                (newCurrentFile, newPrevious) =
                  case model.previous of
                    hd :: tl -> (Just hd, tl)
                    [] ->       (Nothing, [])
              in ( { model | current = Stopped newCurrentFile, previous = newPrevious, next = newNext }, Cmd.none )
            Nothing ->
              ( model, Cmd.none )
        Paused file time ->
          if time > 5 then
            ( { model | current = Paused file 0 }, Cmd.none )
          else
            case model.previous of
              file :: newPrevious ->
                let newNext = file :: model.next
                in ( { model | current = Paused file time, previous = newPrevious, next = newNext }, Cmd.none )
              [] ->
                ( model, Cmd.none )
        Playing file time ->
          if time > 5 then
            ( { model | current = Playing file 0 }, Cmd.none )
          else
            case model.previous of
              file :: newPrevious ->
                let newNext = file :: model.next
                in ( { model | current = Playing file time, previous = newPrevious, next = newNext }, Cmd.none )
              [] ->
                ( model, Cmd.none )
