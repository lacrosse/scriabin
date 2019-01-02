module Connection.Server.Types exposing (..)


type alias Host =
    String


type alias Port =
    String


type alias Endpoint =
    ( Host, Port )


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
