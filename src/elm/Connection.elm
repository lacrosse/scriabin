module Connection exposing (..)

import Connection.Server as Server
import Connection.Server.Types
import LocalStorage


type Model
    = Connected Server.Model
    | Disconnected Connection.Server.Types.Endpoint


initialDisconnected :
    ( Maybe String, Maybe String )
    -> (( String, String ) -> Cmd msg)
    -> ( Model, Cmd msg )
initialDisconnected ( maybeHost, maybePort ) tryConnect =
    case maybeHost of
        Nothing ->
            ( Disconnected ( "", "" ), Cmd.none )

        Just host ->
            case maybePort of
                Nothing ->
                    ( Disconnected ( host, "" ), Cmd.none )

                Just port_ ->
                    let
                        endpoint =
                            ( host, port_ )
                    in
                        ( Disconnected endpoint, tryConnect endpoint )



-- UPDATE


type Msg
    = Disconnect Connection.Server.Types.Endpoint
    | UpdateWannabeHost String
    | UpdateWannabePort String
    | Connect
    | ServerMsg Server.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Disconnect endpoint ->
            case model of
                Disconnected _ ->
                    ( model, Cmd.none )

                Connected server ->
                    ( Disconnected endpoint
                    , Cmd.batch [ LocalStorage.remove "host", LocalStorage.remove "port" ]
                    )

        UpdateWannabeHost host ->
            ( case model of
                Connected _ ->
                    model

                Disconnected ( _, port_ ) ->
                    Disconnected ( host, port_ )
            , Cmd.none
            )

        UpdateWannabePort port_ ->
            ( case model of
                Connected _ ->
                    model

                Disconnected ( host, _ ) ->
                    Disconnected ( host, port_ )
            , Cmd.none
            )

        Connect ->
            case model of
                Connected _ ->
                    ( model, Cmd.none )

                Disconnected ( host, port_ ) ->
                    ( Connected (Server.initialModel ( host, port_ ) Nothing)
                    , Cmd.batch [ LocalStorage.set "host" host, LocalStorage.set "port" port_ ]
                    )

        ServerMsg msg ->
            case model of
                Disconnected _ ->
                    ( model, Cmd.none )

                Connected server ->
                    let
                        ( server_, cmd ) =
                            Server.update msg server
                    in
                        ( Connected server_, Cmd.map ServerMsg cmd )
