module Page.Profile exposing (view)

import Html
    exposing
        ( Html
        , h1
        , p
        , text
        , a
        )
import Html.Attributes exposing (href, target)
import Connection.Server as Server
import I18n exposing (t)
import Page.NotFound
import Components.FontAwesome exposing (fa)
import Lastfm


view : Server.Model -> I18n.Language -> List (Html msg)
view { state } language =
    case state of
        Server.Authenticated user _ ->
            let
                lastfm =
                    p []
                        (fa "lastfm-square"
                            :: (case user.lastfm of
                                    Just lUsername ->
                                        [ a
                                            [ href ("https://last.fm/user/" ++ lUsername)
                                            , target "_blank"
                                            ]
                                            [ text lUsername ]
                                        ]

                                    Nothing ->
                                        [ a
                                            [ href (Lastfm.authUrl "" "")
                                            , target "_blank"
                                            ]
                                            [ text "Authenticate" ]
                                        ]
                               )
                        )
            in
                [ h1 [] (t language I18n.Profile)
                , lastfm
                ]

        Server.NotAuthenticated _ ->
            Page.NotFound.view language
