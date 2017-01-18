module Session exposing (..)

import Http
import Task

-- MODEL

type alias Model =
  Maybe Session

type alias Session =
  { user : User
  }

type alias User =
  { username : String
  }

initialModel : Model
initialModel =
  Nothing

-- UPDATE

type Msg
  = SignIn String String
  | SignInSucceed User
  | SignInFail Http.Error
  | SignOut

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    SignIn username password ->
      ( model, signIn username password )
    SignOut ->
      ( Nothing, Cmd.none )
    SignInSucceed user ->
      ( Just { user = user }, Cmd.none )
    SignInFail error ->
      case error of
        Http.BadPayload message response ->
          ( model, Cmd.none )
        _ ->
          ( model, Cmd.none )

-- HTTP

signIn : String -> String -> Cmd Msg
signIn username password =
  let processSignIn result =
    case result of
      Ok user ->
        SignInSucceed user
      Err error ->
        SignInFail error
  in Task.attempt processSignIn (Task.succeed { username = username })
