module Messages exposing (..)

import Session
import Http
import Components.Flash as Flash
import Routing
import Navigation
import Server
import Components.Player as Player

type Msg
  = Noop
  | SignIn
  | SignInSucceed Session.User
  | SignInFail Http.Error
  | SessionMsg Session.Msg
  | FlashMsg Flash.Msg
  | SetRoute Routing.Route
  | VisitLocation Navigation.Location
  | PlayerMsg Player.Msg
  | ServerMsg Server.Msg
