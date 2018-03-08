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


view : ( String, String ) -> Language -> List (Html Msg)
view ( host, port_ ) language =
    [ h1 [] [ text "Vous êtes déconnecté." ]
    , horizontalForm (ConnectionMsg Connection.Connect)
        [ inputFormGroup
            "server"
            "host"
            "Host"
            "text"
            host
            (ConnectionMsg << Connection.UpdateWannabeHost)
            [ placeholder "localhost" ]
        , inputFormGroup
            "server"
            "port"
            "Port"
            "text"
            port_
            (ConnectionMsg << Connection.UpdateWannabePort)
            [ placeholder "1493" ]
        , div [ class "form-group" ]
            [ div [ class "col-lg-10 col-lg-offset-2" ]
                [ button
                    [ type_ "submit", class "btn btn-primary" ]
                    [ fa "handshake-o", text "Connect" ]
                ]
            ]
        ]
    ]
