module Page.NotFound exposing (view)

import Html
    exposing
        ( Html
        , text
        , h1
        , p
        , div
        )
import I18n exposing (t)


view : I18n.Language -> List (Html msg)
view lang =
    [ div []
        [ h1 [] (t lang I18n.NotFoundHeader)
        , p [] (t lang I18n.TryAgain)
        ]
    ]
