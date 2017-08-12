module Page.NotFound exposing (view)

import Html
    exposing
        ( Html
        , text
        , h1
        , p
        , div
        )


view : List (Html msg)
view =
    [ div []
        [ h1 [] [ text "Not found" ]
        , p [] [ text "Try again." ]
        ]
    ]
