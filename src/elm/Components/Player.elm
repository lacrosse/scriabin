module Components.Player exposing (..)

import Array exposing (Array)
import Models.File exposing (File)

-- MODEL

type Current
  = Stopped
  | Paused File Int
  | Playing File Int

type alias Model =
  { playlist :
    { previous : Array File
    , current : Current
    , next : Array File
    }
  }

initialModel : Model
initialModel =
  { playlist =
    { previous = Array.empty
    , current = Stopped
    , next = Array.empty
    }
  }
