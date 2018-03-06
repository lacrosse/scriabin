module Connection exposing (..)

import Connection.Server as Server
import LocalStorage


type alias Model =
    { currentServer : Maybe Server.Model
    , wannabeEndpoint : String
    }


initialModel : Maybe String -> Maybe String -> Model
initialModel maybeEndpoint maybeToken =
    case maybeEndpoint of
        Nothing ->
            { currentServer = Nothing, wannabeEndpoint = "" }

        Just endpoint ->
            { currentServer = Just (Server.initialModel endpoint maybeToken), wannabeEndpoint = "" }



-- UPDATE


type Msg
    = ServerMsg Server.Msg
    | Disconnect
    | UpdateWannabeEndpoint String
    | Connect


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Disconnect ->
            ( { model | currentServer = Nothing }, LocalStorage.remove "endpoint" )

        UpdateWannabeEndpoint endpoint ->
            ( { model | wannabeEndpoint = endpoint }, Cmd.none )

        Connect ->
            ( { model | currentServer = Just (Server.initialModel model.wannabeEndpoint Nothing) }, LocalStorage.set "endpoint" model.wannabeEndpoint )

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
