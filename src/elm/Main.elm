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
import Html.Attributes exposing (class, type_, attribute, href, target, placeholder)
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


template : Model -> List (Html Msg)
template { language, routing, server } =
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

        authenticatedItems =
            case model.server.state of
                Server.Connected _ _ ->
                    [ li [] [ navLink Routing.Composers [] (t model.language I18n.Composers) ]
                    ]

                Server.Disconnected _ ->
                    []

        leftNav =
            [ ul [ class "nav navbar-nav" ] authenticatedItems ]

        sessionNav =
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
            [ ul [ class "nav navbar-nav navbar-right" ] [ sessionNav ] ]

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
                    ++ [ main_ [ attribute "role" "main" ] (template model) ]
                )

        footer_ =
            footer [ class "container" ]
                [ hr [] []
                , p [] ([ text "Powered by " ] ++ githubLink "lacrosse" "scriabin" ++ [ text " and " ] ++ githubLink "lacrosse" "celeste")
                ]

        player =
            List.map (Html.map PlayerMsg) (Player.view model.player)
    in
        div [] (navbar :: mainContainer :: footer_ :: player)



-- FUNCTIONS


routeToCelesteRoutes : Routing.Route -> List Celeste.Route
routeToCelesteRoutes route =
    case route of
        Routing.Assemblage id _ ->
            [ Celeste.Assemblage id ]

        Routing.Composers ->
            [ Celeste.Composers ]

        _ ->
            []


processLocation : Navigation.Location -> Model -> ( Model, Cmd Msg )
processLocation navLoc model =
    let
        maybeRoute =
            Routing.locationToRoute navLoc

        respToMsg response =
            case response of
                Ok value ->
                    (ServerMsg << Server.StoreRecords << Celeste.responseToTuple) value

                Err (Jwt.HttpError (Http.BadPayload err _)) ->
                    (FlashMsg << Flash.DeriveFromString) err

                Err _ ->
                    Noop

        fetchCmd =
            case maybeRoute of
                Just route ->
                    (Cmd.batch << List.map (Server.authorize model.server Cmd.none << Store.fetch respToMsg model.server.endpoint) << routeToCelesteRoutes) route

                Nothing ->
                    Cmd.none
    in
        ( { model | routing = { currentRoute = maybeRoute } }, fetchCmd )
