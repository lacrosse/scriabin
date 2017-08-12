module Page.Root exposing (..)

import Html exposing (Html, h1, p)
import I18n exposing (..)


view : String -> Language -> List (Html msg)
view server language =
    [ h1 [] (t language Welcome)
    , p [] (t language (YouAreConnectedTo server))
    , p [] (t language NoReasonToPanic)
    ]
