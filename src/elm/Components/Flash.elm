module Components.Flash exposing (..)

import Html exposing (Html, p, text)
import Html.Attributes exposing (class, attribute)
import Json.Decode as JD
import Http

-- MODEL

type Kind
  = Info
  | Error

type alias Flash =
  (Kind, String)

type alias Model =
  Maybe Flash

initialModel : Model
initialModel =
  Nothing

-- UPDATE

type Msg
  = Flush
  | DeriveFromResponse (Http.Response String)
  | DeriveFromString String

update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
  case msg of
    DeriveFromResponse { body } ->
      let
        decoder = JD.decodeString (JD.field "error" JD.string)
        message =
          case decoder body of
            Ok val -> val
            _ -> "Something went wrong!"
        flash = (Error, message)
      in ( Just flash, Cmd.none )
    DeriveFromString string ->
      ( Just (Error, string), Cmd.none )
    Flush ->
      ( Nothing, Cmd.none )

-- VIEW

view : Model -> List (Html msg)
view model =
  let
    paragraph styling string =
      p [ class ("alert alert-" ++ styling), attribute "role" "alert" ] [ text string ]
  in
    case model of
      Just (Info, string) ->
        [paragraph "info" string]
      Just (Error, string) ->
        [paragraph "danger" string]
      Nothing ->
        []
