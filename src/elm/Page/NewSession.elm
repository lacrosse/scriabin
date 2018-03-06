module Page.NewSession exposing (view)

import Html
    exposing
        ( Html
        , text
        , h1
        , div
        , button
        )
import Html.Attributes
    exposing
        ( class
        , placeholder
        , type_
        )
import Connection
import Connection.Server as Server
import I18n exposing (t)
import Messages exposing (..)
import Components.Bootstrap
    exposing
        ( inputFormGroup
        , horizontalForm
        )
import Components.FontAwesome exposing (fa)


view : Server.Model -> I18n.Language -> List (Html Msg)
view { state } language =
    case state of
        Server.NotAuthenticated { username, password } ->
            [ h1 [] (t language I18n.SignIn)
            , horizontalForm SignIn
                [ inputFormGroup
                    "user"
                    "username"
                    "Username"
                    "text"
                    username
                    (ConnectionMsg << Connection.ServerMsg << Server.UpdateWannabeUsername)
                    [ placeholder "seanbooth" ]
                , inputFormGroup
                    "user"
                    "password"
                    "Password"
                    "password"
                    password
                    (ConnectionMsg << Connection.ServerMsg << Server.UpdateWannabePassword)
                    [ placeholder "surripere" ]
                , div [ class "form-group" ]
                    [ div [ class "col-lg-10 col-lg-offset-2" ]
                        [ button
                            [ type_ "submit", class "btn btn-primary" ]
                            (fa "sign-in" :: t language I18n.SignInVerb)
                        ]
                    ]
                ]
            ]

        Server.Authenticated _ _ ->
            [ h1 [] [ text "You are signed in." ] ]
