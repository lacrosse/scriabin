module Session exposing (..)

-- MODEL

type alias Model = Session

type alias Session =
  { user : Maybe User
  , wannabe : Wannabe
  }

type alias User =
  { username : String
  , jwt : String
  }

type alias Wannabe =
  { username : String
  , password : String
  }

initialWannabe : Wannabe
initialWannabe =
  { username = "", password = "" }

initialModel : Model
initialModel =
  { user = Nothing
  , wannabe = initialWannabe
  }

-- UPDATE

type Msg
  = UpdateWannabeUsername String
  | UpdateWannabePassword String
  | FlushWannabe

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    UpdateWannabeUsername value ->
      let
        old = model.wannabe
        new = { old | username = value }
      in ( { model | wannabe = new }, Cmd.none )
    UpdateWannabePassword value ->
      let
        old = model.wannabe
        new = { old | password = value }
      in ( { model | wannabe = new }, Cmd.none )
    FlushWannabe ->
      ( { model | wannabe = initialWannabe }, Cmd.none )
