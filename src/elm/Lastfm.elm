module Lastfm exposing (..)


authUrl : String -> String -> String
authUrl apiKey callbackUrl =
    "https://www.last.fm/api/auth/?api_key=" ++ apiKey ++ "&cb=" ++ callbackUrl
