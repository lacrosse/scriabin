module Connection.Server.Types exposing (..)


type alias User =
    { username : String
    , jwt : String
    , stats : List String
    , lastfm : Maybe String
    }


type alias Wannabe =
    { username : String
    , password : String
    }
