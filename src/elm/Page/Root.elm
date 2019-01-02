module Page.Root exposing (..)

import Html exposing (Html, h1, p, text, a)
import I18n exposing (..)
import Connection.Server.Types exposing (Endpoint)


view : Endpoint -> Language -> List (Html msg)
view endpoint language =
    [ h1 [] (t language Welcome)
    , p [] (t language (YouAreConnectedTo endpoint))
    , p [] (t language NoReasonToPanic)
    ]
