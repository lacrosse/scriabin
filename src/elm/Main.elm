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
import Dict
import Page.Root
import Page.NotFound
import Page.Disconnected
import Page.Assemblage
import Page.Assemblages
import Page.NewSession
import Page.Stats
import Page.Profile
import Components.Html exposing (..)
import Components.FontAwesome exposing (..)
import Components.Flash as Flash
import Components.Player as Player
import Data.Assemblage exposing (Assemblage)
import Routing
import Connection
import Connection.Server as Server
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
    { endpoint : Maybe String
    , token : Maybe String
    }


type alias Model =
    { routing : Routing.Model
    , language : I18n.Language
    , flash : Flash.Model
    , connection : Connection.Model
    , player : Player.Model
    }


init : Flags -> Navigation.Location -> ( Model, Cmd Msg )
init { endpoint, token } navLoc =
    let
        model_ =
            { routing = Routing.initialModel
            , language = I18n.English
            , flash = Flash.initialModel
            , connection = Connection.initialModel endpoint token
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
            case model.connection.currentServer of
                Nothing ->
                    ( model, Cmd.none )

                Just server ->
                    let
                        ( player_, cmd ) =
                            Player.update msg model.player server.endpoint
                    in
                        ( { model | player = player_ }, Cmd.map PlayerMsg cmd )

        ConnectionMsg msg ->
            let
                ( updatedConnection, cmd ) =
                    Connection.update msg model.connection
            in
                ( { model | connection = updatedConnection }, Cmd.map ConnectionMsg cmd )

        HangUp ( outcome, route ) ->
            case outcome of
                Ok celesteTuple ->
                    let
                        ( model_, cmd_ ) =
                            update (ConnectionMsg << Connection.ServerMsg << Server.StoreRecords <| celesteTuple) model

                        ( model__, cmd__ ) =
                            update (RoutingMsg (Routing.FinishTransition (Just route))) model_
                    in
                        ( model__, Cmd.batch [ cmd_, cmd__ ] )

                Err string ->
                    let
                        ( model_, cmd_ ) =
                            update (FlashMsg (Flash.DeriveFromString string)) model

                        ( model__, cmd__ ) =
                            update (RoutingMsg (Routing.FinishTransition (Just route))) model_
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
                    case model.connection.currentServer of
                        Nothing ->
                            Cmd.none

                        Just server ->
                            case server.state of
                                Server.Authenticated _ _ ->
                                    Cmd.none

                                Server.NotAuthenticated wannabe ->
                                    wannabe
                                        |> Server.signIn server.endpoint
                                        |> Http.send responseToMsg
            in
                ( model, cmd )

        SignInSucceed user ->
            let
                ( model_, serverCmd ) =
                    update (ConnectionMsg << Connection.ServerMsg << Server.SignInSucceed <| user) model

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
            case model.connection.currentServer of
                Just server ->
                    withLayout model (routeToPage server model.language route)

                Nothing ->
                    withOverlay model.routing.transitioning <|
                        case route of
                            Routing.Root ->
                                Page.Disconnected.view model.connection.wannabeEndpoint model.language

                            _ ->
                                Page.NotFound.view model.language

        Nothing ->
            withOverlay model.routing.transitioning []


routeToPage : Server.Model -> I18n.Language -> Routing.Route -> List (Html Msg)
routeToPage server language route =
    case route of
        Routing.Root ->
            Page.Root.view server.endpoint language

        Routing.NewSession ->
            Page.NewSession.view server language

        Routing.Stats ->
            Page.Stats.view server language

        Routing.Profile ->
            Page.Profile.view server language

        Routing.Composers ->
            case server.state of
                Server.Authenticated _ store ->
                    store.assemblages
                        |> Dict.filter (always Data.Assemblage.isComposer)
                        |> Dict.toList
                        |> List.map Tuple.second
                        |> List.sortBy .name
                        |> Page.Assemblages.view

                Server.NotAuthenticated _ ->
                    Page.NotFound.view language

        Routing.Assemblage id _ ->
            case server.state of
                Server.Authenticated _ store ->
                    case Dict.get id store.assemblages of
                        Just assemblage ->
                            Page.Assemblage.view server.endpoint assemblage language store

                        Nothing ->
                            Page.NotFound.view language

                Server.NotAuthenticated _ ->
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
            case model.connection.currentServer of
                Nothing ->
                    []

                Just server ->
                    case server.state of
                        Server.NotAuthenticated _ ->
                            []

                        Server.Authenticated _ _ ->
                            [ ul [ class "nav navbar-nav" ]
                                [ li []
                                    [ navLink Routing.Composers [] (t model.language I18n.Composers) ]
                                ]
                            ]

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
            case model.connection.currentServer of
                Nothing ->
                    []

                Just server ->
                    let
                        serverMenu =
                            case server.state of
                                Server.NotAuthenticated _ ->
                                    [ li []
                                        [ navLink Routing.NewSession [] (fa "sign-in" :: text " " :: t model.language I18n.SignIn) ]
                                    ]

                                Server.Authenticated { username } _ ->
                                    [ li [] [ navLink Routing.Profile [] (fa "user" :: t model.language I18n.Profile) ]
                                    , li [] [ navLink Routing.Stats [] (fa "bar-chart" :: t model.language I18n.Stats) ]
                                    , divider
                                    , li []
                                        [ a [ href "#", onClick (ConnectionMsg << Connection.ServerMsg <| Server.SignOut) ]
                                            (fa "sign-out" :: t model.language I18n.SignOut)
                                        ]
                                    ]

                        toggleContent =
                            case server.state of
                                Server.NotAuthenticated _ ->
                                    [ fa "circle-thin", text "..." ]

                                Server.Authenticated { username } _ ->
                                    [ fa "circle", text username ]

                        toggle =
                            a
                                ([ href "#", class "dropdown-toggle", attribute "role" "button" ]
                                    ++ data [ ( "toggle", "dropdown" ) ]
                                    ++ aria [ ( "haspopup", "true" ), ( "expanded", "false" ) ]
                                )
                                toggleContent

                        connectionMenu =
                            [ li []
                                [ a [ href "#", onClick (ConnectionMsg Connection.Disconnect) ]
                                    (fa "chain-broken" :: [ text "Disconnect" ])
                                ]
                            ]

                        divider =
                            li [ class "divider", attribute "role" "separator" ] []

                        dropdownMenu =
                            ul [ class "dropdown-menu" ] (serverMenu ++ divider :: connectionMenu)
                    in
                        [ li [ class "dropdown" ] [ toggle, dropdownMenu ] ]

        navbar =
            nav [ class "navbar navbar-default" ]
                [ div [ class "container" ]
                    [ navbarHeader
                    , div [ class "collapse navbar-collapse" ]
                        (leftNav
                            ++ [ ul [ class "nav navbar-nav navbar-right" ]
                                    -- (languageSwitch ++ authRightNavContent)
                                    authRightNavContent
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
            footer [ class "container" ] []

        -- [ hr [] []
        -- , p []
        --     (text "Powered by " :: githubLink "lacrosse" "scriabin" ++ text " and " :: githubLink "lacrosse" "celeste")
        -- ]
        player =
            List.map (Html.map PlayerMsg) (Player.view model.player)
    in
        withOverlay model.routing.transitioning (navbar :: mainContainer :: footer_ :: player)



-- FUNCTIONS


celesteRouteForAppRoute : Routing.Route -> Maybe Celeste.Route
celesteRouteForAppRoute route =
    case route of
        Routing.Assemblage id _ ->
            Just (Celeste.Assemblage id)

        Routing.Composers ->
            Just (Celeste.Composers)

        _ ->
            Nothing


processLocation : Navigation.Location -> Model -> ( Model, Cmd Msg )
processLocation location model =
    case model.connection.currentServer of
        Nothing ->
            update (RoutingMsg <| Routing.FinishTransition <| Just Routing.Root) model

        Just server ->
            let
                maybeAppRoute =
                    Routing.locationToRoute location

                handleAppRoute route =
                    let
                        resultHandler cRes =
                            HangUp ( Celeste.resultToOutcome cRes, route )
                    in
                        route
                            |> celesteRouteForAppRoute
                            |> Maybe.map (\celesteRoute -> ( celesteRoute, resultHandler ))

                fetchWrapper fetch endpoint jwt =
                    maybeAppRoute
                        |> Maybe.andThen handleAppRoute
                        |> Maybe.andThen (\( celesteRoute, resultHandler ) -> fetch celesteRoute resultHandler endpoint jwt)
            in
                case Server.maybeAuthenticatedFetchAndHandle fetchWrapper server of
                    Just fetchAndHandleCmd ->
                        let
                            ( model_, startTransitionCmd ) =
                                update (RoutingMsg Routing.StartTransition) model
                        in
                            ( model_, Cmd.batch [ startTransitionCmd, fetchAndHandleCmd ] )

                    Nothing ->
                        update (RoutingMsg <| Routing.FinishTransition maybeAppRoute) model
