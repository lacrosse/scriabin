module Connection exposing (..)

import Connection.Server as Server
import LocalStorage


type alias Model =
    { currentServer : Maybe Server.Model
    , wannabeEndpoint : ( String, String )
    }


initialModel : ( Maybe String, Maybe String ) -> Maybe String -> Model
initialModel ( maybeHost, maybePort ) maybeToken =
    case maybeHost of
        Nothing ->
            { currentServer = Nothing, wannabeEndpoint = ( "", "" ) }

        Just host ->
            { currentServer = Just (Server.initialModel ( host, maybePort ) maybeToken), wannabeEndpoint = ( "", "" ) }



-- UPDATE


type Msg
    = ServerMsg Server.Msg
    | Disconnect
    | UpdateWannabeHost String
    | UpdateWannabePort String
    | Connect


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Disconnect ->
            ( { model | currentServer = Nothing }, LocalStorage.remove "endpoint" )

        UpdateWannabeHost host ->
            let
                ( _, port_ ) =
                    model.wannabeEndpoint
            in
                ( { model | wannabeEndpoint = ( host, port_ ) }, Cmd.none )

        UpdateWannabePort port_ ->
            let
                ( host, _ ) =
                    model.wannabeEndpoint
            in
                ( { model | wannabeEndpoint = ( host, port_ ) }, Cmd.none )

        Connect ->
            let
                ( host, port_ ) =
                    model.wannabeEndpoint

                localStorageCmd =
                    Cmd.batch
                        [ LocalStorage.set "host" host
                        , LocalStorage.set "port" port_
                        ]
            in
                ( { model | currentServer = Just (Server.initialModel ( host, Just port_ ) Nothing) }, localStorageCmd )

        ServerMsg msg ->
            case model.currentServer of
                Just server ->
                    let
                        ( server_, cmd ) =
                            Server.update msg server
                    in
                        ( { model | currentServer = Just server_ }, Cmd.map ServerMsg cmd )

                Nothing ->
                    ( model, Cmd.none )
