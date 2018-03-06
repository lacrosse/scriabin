module Page.Disconnected exposing (..)

import Html
    exposing
        ( Html
        , h1
        , div
        , p
        , text
        , a
        , button
        )
import Html.Attributes
    exposing
        ( class
        , type_
        , placeholder
        )
import I18n exposing (..)
import Messages exposing (..)
import Connection
import Components.FontAwesome exposing (fa)
import Components.Bootstrap
    exposing
        ( inputFormGroup
        , horizontalForm
        )


view : String -> Language -> List (Html Msg)
view endpoint language =
    [ h1 [] [ text "Vous êtes déconnecté." ]
    , horizontalForm (ConnectionMsg Connection.Connect)
        [ inputFormGroup
            "server"
            "endpoint"
            "Endpoint"
            "text"
            endpoint
            (ConnectionMsg << Connection.UpdateWannabeEndpoint)
            [ placeholder "https://localhost:9000/api" ]
        , div [ class "form-group" ]
            [ div [ class "col-lg-10 col-lg-offset-2" ]
                [ button
                    [ type_ "submit", class "btn btn-primary" ]
                    [ fa "handshake-o", text "Connect" ]
                ]
            ]
        ]
    ]
