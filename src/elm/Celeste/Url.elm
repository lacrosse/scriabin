module Celeste.Url exposing (..)

import Connection.Server.Types exposing (Endpoint)


-- URLS


server : Endpoint -> String
server ( host, port_ ) =
    let
        hostWithPort =
            if String.isEmpty port_ then
                host
            else
                host ++ ":" ++ port_
    in
        "http://" ++ hostWithPort


api : Endpoint -> String
api endpoint =
    server endpoint ++ "/api"
