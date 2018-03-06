module Messages exposing (..)

import Http
import Components.Flash as Flash
import Routing
import Navigation
import Connection
import Connection.Server as Server
import Celeste
import I18n
import Components.Player as Player


type Msg
    = Noop
    | SetLanguage I18n.Language
    | SignIn
    | SignInSucceed Server.User
    | SignInFail Http.Error
    | FlashMsg Flash.Msg
    | HangUp ( Celeste.Outcome, Routing.Route )
    | SetRoute Routing.Route
    | RoutingMsg Routing.Msg
    | VisitLocation Navigation.Location
    | PlayerMsg Player.Msg
    | ConnectionMsg Connection.Msg
