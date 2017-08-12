module Messages exposing (..)

import Http
import Components.Flash as Flash
import Routing
import Navigation
import Server
import Celeste
import I18n
import Components.Player as Player


type alias ConversationOutcome =
    Result String ( Celeste.ResponseTuple, Routing.Route )


type Msg
    = Noop
    | SetLanguage I18n.Language
    | SignIn
    | SignInSucceed Server.User
    | SignInFail Http.Error
    | FlashMsg Flash.Msg
    | HangUp ConversationOutcome
    | SetRoute Routing.Route
    | RoutingMsg Routing.Msg
    | VisitLocation Navigation.Location
    | PlayerMsg Player.Msg
    | ServerMsg Server.Msg
