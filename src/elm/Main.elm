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
        , h1
        , h3
        , h4
        , p
        , ul
        , li
        , table
        , thead
        , tbody
        , tr
        , th
        , td
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
import Html.Events exposing (onClick, onWithOptions)
import Http
import Navigation
import Regex
import Jwt
import Dict
import Page.Root
import Page.Assemblage
import Components.Html exposing (data, aria)
import Components.FontAwesome exposing (fa, faText)
import Components.Flash as Flash
import Components.Bootstrap exposing (horizontalForm, inputFormGroup)
import Components.Player as Player
import Data.Assemblage as Assemblage exposing (Assemblage)
import Routing
import Server
import Celeste
import Json.Decode as JD
import View.Common
    exposing
        ( notFound
        , statsView
        , tagLabel
        , tagsRow
        , navLink
        , threeBars
        , githubLink
        )
import Store exposing (Store, assemblagesThroughAssemblies)
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
                cmd =
                    case model.server.state of
                        Server.Disconnected wannabe ->
                            wannabe
                                |> Server.signIn model.server.endpoint
                                |> Http.send
                                    (\result ->
                                        case result of
                                            Ok user ->
                                                SignInSucceed user

                                            Err error ->
                                                SignInFail error
                                    )

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


modelToTemplate : Model -> List (Html Msg)
modelToTemplate { language, routing, server } =
    case routing.currentRoute of
        Just (Routing.Root) ->
            Page.Root.view server.endpoint language

        Just (Routing.NewSession) ->
            newSessionView server language

        Just (Routing.Stats) ->
            statsView server

        Just (Routing.Composers) ->
            case server.state of
                Server.Connected _ store ->
                    let
                        assemblagesStore =
                            Dict.filter (\_ a -> Assemblage.isComposer a) store.assemblages

                        assemblages =
                            (List.sortBy .name << List.map Tuple.second << Dict.toList) assemblagesStore
                    in
                        assemblagesView assemblages

                Server.Disconnected _ ->
                    [ notFound ]

        Just (Routing.Assemblage id _) ->
            case server.state of
                Server.Connected _ store ->
                    case Dict.get id store.assemblages of
                        Just assemblage ->
                            Page.Assemblage.view language assemblage store

                        Nothing ->
                            [ notFound ]

                Server.Disconnected _ ->
                    [ notFound ]

        Nothing ->
            [ notFound ]


newSessionView : Server.Model -> I18n.Language -> List (Html Msg)
newSessionView { state } language =
    case state of
        Server.Disconnected { username, password } ->
            [ h1 [] (t language I18n.SignIn)
            , horizontalForm SignIn
                [ inputFormGroup "user"
                    "username"
                    "Username"
                    "text"
                    username
                    (ServerMsg << Server.UpdateWannabeUsername)
                    [ placeholder "seanbooth" ]
                , inputFormGroup "user"
                    "password"
                    "Password"
                    "password"
                    password
                    (ServerMsg << Server.UpdateWannabePassword)
                    [ placeholder "6IE.CR" ]
                , div [ class "form-group" ]
                    [ div [ class "col-lg-10 col-lg-offset-2" ]
                        [ button [ type_ "submit", class "btn btn-primary" ] (faText "sign-in" "Sign In") ]
                    ]
                ]
            ]

        Server.Connected _ _ ->
            [ h1 [] [ text "You are signed in." ] ]


assemblagesView : List Assemblage -> List (Html Msg)
assemblagesView =
    flip (::) [] << div [] << List.map assemblageRowView


assemblageRowView : Assemblage -> Html Msg
assemblageRowView =
    p [] << List.singleton << View.Common.assemblageLink


compositionRowView : Assemblage -> Html Msg
compositionRowView =
    assemblageRowView


wikipediaPath : String -> String
wikipediaPath =
    (++) "https://en.wikipedia.org/wiki/" << Regex.replace Regex.All (Regex.regex " ") (always "_")


view : Model -> Html Msg
view model =
    withLayout model (modelToTemplate model)


withLayout : Model -> List (Html Msg) -> Html Msg
withLayout model content =
    let
        overlay =
            case model.routing.transitioning of
                True ->
                    [ div
                        [ class "overlay-main" ]
                        [ div
                            [ class "la-ball-grid-beat la-dark la-2x overlay-loader" ]
                            (List.repeat 9 (div [] []))
                        ]
                    ]

                False ->
                    []

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

        rightNavContent =
            case model.server.state of
                Server.Connected { username } _ ->
                    li [ class "dropdown" ]
                        [ a
                            ([ href "#", class "dropdown-toggle", attribute "role" "button" ]
                                ++ data [ ( "toggle", "dropdown" ) ]
                                ++ aria [ ( "haspopup", "true" ), ( "expanded", "false" ) ]
                            )
                            (faText "user-circle-o" username)
                        , ul [ class "dropdown-menu" ]
                            [ li []
                                [ navLink Routing.Stats
                                    []
                                    (fa "bar-chart" :: t model.language I18n.Stats)
                                ]
                            , li [ class "divider", attribute "role" "separator" ] []
                            , li []
                                (List.map
                                    (\( lang, desc ) ->
                                        a
                                            [ onWithOptions "click"
                                                { stopPropagation = True
                                                , preventDefault = True
                                                }
                                                (JD.succeed (SetLanguage lang))
                                            ]
                                            [ text desc ]
                                    )
                                    [ ( I18n.English, "🇺🇸 English" )
                                    , ( I18n.Russian, "🇷🇺 Russian" )
                                    ]
                                )
                            , li [ class "divider", attribute "role" "separator" ] []
                            , li []
                                [ a [ href "#", onClick (ServerMsg Server.SignOut) ]
                                    (fa "sign-out" :: t model.language I18n.SignOut)
                                ]
                            ]
                        ]

                Server.Disconnected _ ->
                    li []
                        [ navLink Routing.NewSession [] (faText "sign-in" "Sign In")
                        ]

        rightNav =
            [ ul [ class "nav navbar-nav navbar-right" ] [ rightNavContent ] ]

        navbar =
            nav [ class "navbar navbar-default" ]
                [ div [ class "container" ]
                    [ navbarHeader
                    , div [ class "collapse navbar-collapse" ] (leftNav ++ rightNav)
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
                    ([ text "Powered by " ]
                        ++ githubLink "lacrosse" "scriabin"
                        ++ [ text " and " ]
                        ++ githubLink "lacrosse" "celeste"
                    )
                ]

        player =
            List.map (Html.map PlayerMsg) (Player.view model.player)
    in
        div [] (overlay ++ navbar :: mainContainer :: footer_ :: player)



-- FUNCTIONS


processLocation : Navigation.Location -> Model -> ( Model, Cmd Msg )
processLocation navLoc model =
    let
        routeToCelesteRoute route =
            case route of
                Routing.Assemblage id _ ->
                    Just (Celeste.Assemblage id)

                Routing.Composers ->
                    Just Celeste.Composers

                _ ->
                    Nothing

        maybeRoute =
            Routing.locationToRoute navLoc

        respToMsg route response =
            case response of
                Ok value ->
                    HangUp (Ok ( Celeste.responseToTuple value, route ))

                Err err ->
                    let
                        string =
                            case err of
                                Jwt.HttpError (Http.BadPayload val _) ->
                                    val

                                error ->
                                    toString error
                    in
                        HangUp (Err string)
    in
        case maybeRoute of
            Just route ->
                case routeToCelesteRoute route of
                    Just cRoute ->
                        let
                            mCmd =
                                Server.authorize
                                    model.server
                                    (Store.fetch (respToMsg route) model.server.endpoint cRoute)
                        in
                            case mCmd of
                                Just cmd ->
                                    let
                                        ( model_, routingCmd ) =
                                            update (RoutingMsg Routing.StartTransition) model
                                    in
                                        ( model_, Cmd.batch [ routingCmd, cmd ] )

                                Nothing ->
                                    ( model, Cmd.none )

                    Nothing ->
                        ( { model | routing = { currentRoute = maybeRoute, transitioning = False } }, Cmd.none )

            Nothing ->
                ( { model | routing = { currentRoute = maybeRoute, transitioning = False } }, Cmd.none )
