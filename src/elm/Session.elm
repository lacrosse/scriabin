module Session exposing (..)

import Http
import Json.Encode as JE
import Json.Decode as JD
import Jwt

import LocalStorage

-- MODEL

type alias Model = Session

type Session
  = Present User
  | Blank Wannabe

type alias User =
  { username : String
  , jwt : String
  , stats : List String
  }

type alias Wannabe =
  { username : String
  , password : String
  }

initialWannabe : Wannabe
initialWannabe =
  { username = "", password = "" }

initialModel : Maybe String -> Model
initialModel maybeJwt =
  case maybeJwt of
    Just jwt ->
      (Present << fetchUser) jwt
    Nothing ->
      Blank initialWannabe

-- UPDATE

type Msg
  = UpdateWannabeUsername String
  | UpdateWannabePassword String
  | FlushWannabe
  | SignInSucceed User
  | SignOut

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    UpdateWannabeUsername value ->
      case model of
        Blank wannabe ->
          (Blank { wannabe | username = value }, Cmd.none)
        Present _ ->
          (model, Cmd.none)
    UpdateWannabePassword value ->
      case model of
        Blank wannabe ->
          (Blank { wannabe | password = value }, Cmd.none)
        Present _ ->
          (model, Cmd.none)
    FlushWannabe ->
      case model of
        Blank wannabe ->
          (Blank initialWannabe, Cmd.none)
        Present _ ->
          (model, Cmd.none)
    SignInSucceed user ->
      (Present user, LocalStorage.set "token" user.jwt)
    SignOut ->
      (Blank initialWannabe, LocalStorage.remove "token")

-- FUNCTIONS

signIn : String -> Wannabe -> Http.Request User
signIn server { username, password } =
  let
    payload =
      JE.object
        [ ( "session"
          , JE.object
            [ ("username", JE.string username)
            , ("password", JE.string password)
            ]
          )
        ]
    decoder =
      JD.field "session"
        (JD.map2
          (\u j -> User u j [])
          (JD.field "username" JD.string)
          (JD.field "jwt" JD.string))
  in Jwt.authenticate (server ++ "/session") decoder payload

authorize : Session -> a -> (String -> a) -> a
authorize session none function =
  case session of
    Present { jwt } -> function jwt
    Blank _ -> none

fetchUser : String -> User
fetchUser jwt =
  { username = ""
  , stats = []
  , jwt = jwt
  }
