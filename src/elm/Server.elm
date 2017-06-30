module Server exposing (..)

import Http
import Json.Encode as JE
import Json.Decode as JD
import Jwt

import Store
import Celeste
import LocalStorage

type alias User =
  { username : String
  , jwt : String
  , stats : List String
  }

type alias Wannabe =
  { username : String
  , password : String
  }

type State
  = Disconnected Wannabe
  | Connected User Store.Model

type alias Model =
  { endpoint : String
  , state : State
  }

initialWannabe : Wannabe
initialWannabe =
  { username = "", password = "" }

initialUser : String -> Maybe String -> Maybe User
initialUser endpoint mToken =
  mToken |> Maybe.andThen (fetchUser endpoint)

initialModel : String -> Maybe String -> Model
initialModel serverRoot token =
  let
    endpoint = serverRoot ++ "/api"
    state =
      case initialUser endpoint token of
        Just user ->
          Connected user Store.initialModel
        Nothing ->
          Disconnected initialWannabe
  in { endpoint = endpoint, state = state }

-- UPDATE

type Msg
  = UpdateWannabeUsername String
  | UpdateWannabePassword String
  | FlushWannabe
  | SignInSucceed User
  | SignOut
  | StoreRecords Celeste.ResponseTuple

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    UpdateWannabeUsername value ->
      case model.state of
        Disconnected wannabe ->
          ({ model | state = Disconnected { wannabe | username = value } }, Cmd.none)
        Connected _ _ ->
          (model, Cmd.none)
    UpdateWannabePassword value ->
      case model.state of
        Disconnected wannabe ->
          ({ model | state = Disconnected { wannabe | password = value } }, Cmd.none)
        Connected _ _ ->
          (model, Cmd.none)
    FlushWannabe ->
      case model.state of
        Disconnected _ ->
          ({ model | state = Disconnected initialWannabe }, Cmd.none)
        Connected _ _ ->
          (model, Cmd.none)
    SignInSucceed user ->
      ({ model | state = Connected user Store.initialModel }, LocalStorage.set "token" user.jwt)
    SignOut ->
      ({ model | state = Disconnected initialWannabe }, LocalStorage.remove "token")
    StoreRecords tuple ->
      case model.state of
        Disconnected _ ->
          (model, Cmd.none)
        Connected user store ->
          let (updatedStore, cmd) = Store.update tuple store
          in ({ model | state = Connected user updatedStore }, cmd)

-- MESSAGES

authorize : Model -> a -> (String -> a) -> a
authorize { state } none function =
  case state of
    Connected { jwt } _ -> function jwt
    Disconnected _ -> none

-- FUNCTIONS

fetchUser : String -> String -> Maybe User
fetchUser endpoint jwt =
  let
    user =
      { username = ""
      , stats = []
      , jwt = jwt
      }
  in Just user

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
