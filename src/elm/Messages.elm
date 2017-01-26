module Messages exposing (..)

import Session
import Http
import Components.Flash as Flash
import Routing
import Navigation
import Celeste

type Msg
  = Noop
  | SignIn
  | SignInSucceed Session.User
  | SignInFail Http.Error
  | SignOut
  | SessionMsg Session.Msg
  | FlashMsg Flash.Msg
  | SetRoute Routing.Route
  | VisitLocation Navigation.Location
  | StoreRecords Celeste.ResponseTuple
