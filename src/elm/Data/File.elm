module Data.File exposing (..)

import Json.Decode as JD
import Json.Encode as JE
import Connection.Server.Types exposing (Endpoint)
import Celeste.Url as Url


-- MODEL


type alias File =
    { id : Int
    , path : String
    , mime : String
    , name : String
    }



-- DECODERS


jsonDecoder : JD.Decoder File
jsonDecoder =
    JD.map4 File
        (JD.field "id" JD.int)
        (JD.field "path" JD.string)
        (JD.field "mime" JD.string)
        (JD.field "name" JD.string)



-- FUNCTIONS


url : Endpoint -> File -> String
url endpoint file =
    Url.server endpoint ++ "/files/" ++ file.path


toJsonList : Endpoint -> File -> List ( String, JE.Value )
toJsonList endpoint file =
    [ ( "url", JE.string << url endpoint <| file )
    , ( "id", JE.int << .id <| file )
    ]
