module Server exposing (..)

import Store
import Celeste

type alias Contents =
  { store : Store.Model
  }

type State
  = Disconnected
  | Connected Contents

type alias Model =
  { endpoint : String
  , state : State
  }

initialModel : String -> Model
initialModel serverRoot =
  { endpoint = serverRoot ++ "/api"
  , state = Disconnected
  }

-- UPDATE

type Msg
  = StoreRecords Celeste.ResponseTuple

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    StoreRecords tuple ->
      case model.state of
        Disconnected ->
          (model, Cmd.none)
        Connected contents ->
          let
            (updatedStore, cmd) = Store.update tuple contents.store
            updatedContents = { contents | store = updatedStore }
            updatedState = Connected updatedContents
          in ({ model | state = updatedState }, cmd)
