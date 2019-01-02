module Messages exposing (..)

import Http
import Components.Flash as Flash
import Routing
import Navigation
import Connection
import Connection.Server.Types as ServerTypes
import Celeste
import I18n
import Components.Player as Player


type Msg
    = Noop
    | SetLanguage I18n.Language
    | ConnectionMsg Connection.Msg
    | ConnectSucceed ServerTypes.Endpoint
    | ConnectFail ServerTypes.Endpoint
    | SignIn
    | SignInSucceed ServerTypes.User
    | SignInFail Http.Error
    | FlashMsg Flash.Msg
    | HangUp ( Celeste.Outcome, Routing.Route )
    | SetRoute Routing.Route
    | RoutingMsg Routing.Msg
    | VisitLocation Navigation.Location
    | PlayerMsg Player.Msg
