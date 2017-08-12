module Page.Assemblage exposing (..)

import Html exposing (Html)
import I18n
import Messages exposing (Msg)
import Data.Assemblage as Assemblage exposing (Assemblage)
import Page.Person
import Page.Composition
import Page.Performance
import Store exposing (Store)


view : I18n.Language -> Assemblage -> Store -> List (Html Msg)
view language assemblage =
    case assemblage.kind of
        Assemblage.Person ->
            Page.Person.view language assemblage

        Assemblage.Composition ->
            Page.Composition.view assemblage

        Assemblage.Recording ->
            Page.Performance.view assemblage

        Assemblage.General ->
            Page.Person.view language assemblage
