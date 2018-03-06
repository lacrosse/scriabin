module Page.Root exposing (..)

import Html exposing (Html, h1, p, text, a)
import I18n exposing (..)


view : String -> Language -> List (Html msg)
view endpoint language =
    [ h1 [] (t language Welcome)
    , p [] (t language (YouAreConnectedTo endpoint))
    , p [] (t language NoReasonToPanic)
    ]
