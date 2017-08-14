module Main exposing (main)

import Html
    exposing
        ( Html
        , Attribute
        , div
        , main_
        , nav
        , footer
        , text
        , a
        , span
        , button
        , p
        , ul
        , li
        , hr
        )
import Html.Attributes
    exposing
        ( class
        , type_
        , attribute
        , href
        , target
        , placeholder
        )
import Html.Events
    exposing
        ( onClick
        , onWithOptions
        )
import Http
import Navigation
import Jwt
import Dict
import Page.Root
import Page.NotFound
import Page.Assemblage
import Page.Assemblages
import Page.NewSession
import Page.Stats
import Components.Html exposing (..)
import Components.FontAwesome exposing (..)
import Components.Flash as Flash
import Components.Player as Player
import Data.Assemblage exposing (Assemblage)
import Routing
import Server
import Celeste
import Json.Decode as JD
import View.Common
    exposing
        ( tagLabel
        , tagsRow
        , navLink
        , threeBars
        , githubLink
        )
import Store
import Messages exposing (..)
import I18n exposing (t)


main : Program Flags Model Msg
main =
    Navigation.programWithFlags VisitLocation
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Flags =
    { token : Maybe String
    }


type alias Model =
    { routing : Routing.Model
    , language : I18n.Language
    , flash : Flash.Model
    , server : Server.Model
    , player : Player.Model
    }


init : Flags -> Navigation.Location -> ( Model, Cmd Msg )
init { token } navLoc =
    let
        model_ =
            { routing = Routing.initialModel
            , language = I18n.English
            , flash = Flash.initialModel
            , server = Server.initialModel navLoc.origin token
            , player = Player.initialModel
            }

        ( model, navCmd ) =
            processLocation navLoc model_
    in
        ( model, navCmd )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            ( model, Cmd.none )

        SetLanguage lang ->
            ( { model | language = lang }, Cmd.none )

        FlashMsg msg ->
            let
                ( updatedFlash, cmd ) =
                    Flash.update msg model.flash
            in
                ( { model | flash = updatedFlash }, Cmd.map FlashMsg cmd )

        PlayerMsg msg ->
            let
                ( updatedPlayer, cmd ) =
                    Player.update msg model.player model.server.endpoint
            in
                ( { model | player = updatedPlayer }, Cmd.map PlayerMsg cmd )

        ServerMsg msg ->
            let
                ( updatedServer, cmd ) =
                    Server.update msg model.server
            in
                ( { model | server = updatedServer }, Cmd.map ServerMsg cmd )

        HangUp outcome ->
            case outcome of
                Ok ( celesteTuple, route ) ->
                    let
                        ( model_, cmd_ ) =
                            update (ServerMsg (Server.StoreRecords celesteTuple)) model

                        ( model__, cmd__ ) =
                            update (RoutingMsg (Routing.FinishTransition (Just route))) model_
                    in
                        ( model__, Cmd.batch [ cmd_, cmd__ ] )

                Err error ->
                    let
                        ( model_, cmd_ ) =
                            update (FlashMsg (Flash.DeriveFromString error)) model

                        ( model__, cmd__ ) =
                            update (RoutingMsg (Routing.FinishTransition Nothing)) model_
                    in
                        ( model__, Cmd.batch [ cmd_, cmd__ ] )

        SetRoute route ->
            ( model, Navigation.newUrl (Routing.routeToString route) )

        VisitLocation navLoc ->
            let
                ( model_, fetchCmd ) =
                    processLocation navLoc model

                ( model__, flashCmd ) =
                    update (FlashMsg Flash.Flush) model_
            in
                ( model__, Cmd.batch [ fetchCmd, flashCmd ] )

        RoutingMsg msg ->
            let
                ( routing_, cmd ) =
                    Routing.update msg model.routing
            in
                ( { model | routing = routing_ }, Cmd.map RoutingMsg cmd )

        SignIn ->
            let
                responseToMsg resp =
                    case resp of
                        Ok user ->
                            SignInSucceed user

                        Err error ->
                            SignInFail error

                cmd =
                    case model.server.state of
                        Server.Disconnected wannabe ->
                            wannabe
                                |> Server.signIn model.server.endpoint
                                |> Http.send responseToMsg

                        Server.Connected _ _ ->
                            Cmd.none
            in
                ( model, cmd )

        SignInSucceed user ->
            let
                ( model_, serverCmd ) =
                    update (ServerMsg (Server.SignInSucceed user)) model

                ( model__, redirect ) =
                    update (SetRoute Routing.Root) model_
            in
                ( model__, Cmd.batch [ serverCmd, redirect ] )

        SignInFail error ->
            case error of
                Http.BadStatus resp ->
                    (update << FlashMsg << Flash.DeriveFromResponse) resp model

                _ ->
                    ( model, Cmd.none )



-- SUB


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [ Sub.map PlayerMsg (Player.subscriptions model.player) ]



-- VIEW


view : Model -> Html Msg
view model =
    case model.routing.currentRoute of
        Just route ->
            withLayout model (routeToPage route model)

        Nothing ->
            withOverlay model.routing.transitioning []


routeToPage : Routing.Route -> Model -> List (Html Msg)
routeToPage route { language, server } =
    case route of
        Routing.Root ->
            Page.Root.view server.endpoint language

        Routing.NewSession ->
            Page.NewSession.view server language

        Routing.Stats ->
            Page.Stats.view server language

        Routing.Composers ->
            case server.state of
                Server.Connected _ store ->
                    store.assemblages
                        |> Dict.filter (always Data.Assemblage.isComposer)
                        |> Dict.toList
                        |> List.map Tuple.second
                        |> List.sortBy .name
                        |> Page.Assemblages.view

                Server.Disconnected _ ->
                    Page.NotFound.view language

        Routing.Assemblage id _ ->
            case server.state of
                Server.Connected _ store ->
                    case Dict.get id store.assemblages of
                        Just assemblage ->
                            Page.Assemblage.view assemblage language store

                        Nothing ->
                            Page.NotFound.view language

                Server.Disconnected _ ->
                    Page.NotFound.view language


withOverlay : Bool -> List (Html msg) -> Html msg
withOverlay transitioning content =
    let
        overlay =
            div
                [ class "overlay-main" ]
                [ div
                    [ class "la-ball-grid-beat la-dark la-2x overlay-loader" ]
                    (List.repeat 9 (div [] []))
                ]

        overlaidContent =
            if transitioning then
                overlay :: content
            else
                content
    in
        div [] overlaidContent


withLayout : Model -> List (Html Msg) -> Html Msg
withLayout model content =
    let
        navbarHeader =
            div [ class "navbar-header" ]
                [ button
                    ([ type_ "button"
                     , class "navbar-toggle collapsed"
                     ]
                        ++ (data [ ( "toggle", "collapse" ) ])
                        ++ (aria [ ( "expanded", "false" ) ])
                    )
                    (span [ class "sr-only" ] [ text "Toggle navigation" ] :: threeBars)
                , navLink Routing.Root [ class "navbar-brand" ] [ text "Celeste" ]
                ]

        leftNav =
            case model.server.state of
                Server.Connected _ _ ->
                    [ ul [ class "nav navbar-nav" ]
                        [ li []
                            [ navLink Routing.Composers [] (t model.language I18n.Composers) ]
                        ]
                    ]

                Server.Disconnected _ ->
                    []

        languageRow lang =
            li []
                [ a
                    [ onWithOptions "click"
                        { stopPropagation = True
                        , preventDefault = True
                        }
                        (JD.succeed (SetLanguage lang))
                    ]
                    [ text << I18n.languageDescription model.language <| lang ]
                ]

        currentLanguageDescription =
            I18n.languageDescription model.language model.language

        languageSwitch =
            [ li [ class "dropdown" ]
                [ a
                    ([ href "#", class "dropdown-toggle", attribute "role" "button" ]
                        ++ data [ ( "toggle", "dropdown" ) ]
                        ++ aria [ ( "haspopup", "true" ), ( "expanded", "false" ) ]
                    )
                    [ text currentLanguageDescription ]
                , ul [ class "dropdown-menu" ]
                    (List.map languageRow << List.filter ((/=) model.language) <| [ I18n.English, I18n.Russian ])
                ]
            ]

        authRightNavContent =
            case model.server.state of
                Server.Connected { username } _ ->
                    [ li [ class "dropdown" ]
                        [ a
                            ([ href "#", class "dropdown-toggle", attribute "role" "button" ]
                                ++ data [ ( "toggle", "dropdown" ) ]
                                ++ aria [ ( "haspopup", "true" ), ( "expanded", "false" ) ]
                            )
                            [ fa "user-circle-o", text username ]
                        , ul [ class "dropdown-menu" ]
                            [ li []
                                [ navLink Routing.Stats
                                    []
                                    (fa "bar-chart" :: t model.language I18n.Stats)
                                ]
                            , li [ class "divider", attribute "role" "separator" ] []
                            , li []
                                [ a [ href "#", onClick (ServerMsg Server.SignOut) ]
                                    (fa "sign-out" :: t model.language I18n.SignOut)
                                ]
                            ]
                        ]
                    ]

                Server.Disconnected _ ->
                    [ li []
                        [ navLink Routing.NewSession [] (fa "sign-in" :: text " " :: t model.language I18n.SignIn) ]
                    ]

        navbar =
            nav [ class "navbar navbar-default" ]
                [ div [ class "container" ]
                    [ navbarHeader
                    , div [ class "collapse navbar-collapse" ]
                        (leftNav
                            ++ [ ul [ class "nav navbar-nav navbar-right" ]
                                    (languageSwitch ++ authRightNavContent)
                               ]
                        )
                    ]
                ]

        mainContainer =
            div [ class "container" ]
                (Flash.view model.flash
                    ++ [ main_ [ attribute "role" "main" ] content ]
                )

        footer_ =
            footer [ class "container" ]
                [ hr [] []
                , p []
                    (text "Powered by " :: githubLink "lacrosse" "scriabin" ++ text " and " :: githubLink "lacrosse" "celeste")
                ]

        player =
            List.map (Html.map PlayerMsg) (Player.view model.player)
    in
        withOverlay model.routing.transitioning (navbar :: mainContainer :: footer_ :: player)



-- FUNCTIONS


respToMsg : Routing.Route -> Result Jwt.JwtError Celeste.Response -> Msg
respToMsg route response =
    case response of
        Ok value ->
            HangUp <| Ok <| ( Celeste.responseToTuple value, route )

        Err err ->
            HangUp <|
                Err <|
                    case err of
                        Jwt.HttpError (Http.BadPayload val _) ->
                            val

                        error ->
                            toString error


routeToCelesteRoute : Routing.Route -> Maybe ( Celeste.Route, Result Jwt.JwtError Celeste.Response -> Msg )
routeToCelesteRoute route =
    Maybe.map (flip (,) (respToMsg route)) <|
        case route of
            Routing.Assemblage id _ ->
                Just (Celeste.Assemblage id)

            Routing.Composers ->
                Just (Celeste.Composers)

            _ ->
                Nothing


processLocation : Navigation.Location -> Model -> ( Model, Cmd Msg )
processLocation navLoc model =
    let
        appRoute =
            Routing.locationToRoute navLoc

        fetch ( cRoute, msg ) =
            cRoute
                |> Store.fetch msg model.server.endpoint
                |> Server.authorize model.server
    in
        case
            appRoute
                |> Maybe.andThen routeToCelesteRoute
                |> Maybe.andThen fetch
        of
            Just cmd ->
                let
                    ( model_, routingCmd ) =
                        update (RoutingMsg Routing.StartTransition) model
                in
                    ( model_, Cmd.batch [ routingCmd, cmd ] )

            Nothing ->
                ( { model | routing = { currentRoute = appRoute, transitioning = False } }, Cmd.none )
