module Page.Assemblage exposing (..)

import Html exposing (Html)
import I18n
import Messages exposing (Msg)
import Data.Assemblage as Assemblage exposing (Assemblage)
import Page.Person
import Page.Composition
import Page.Performance
import Store exposing (Store)


view : String -> Assemblage -> I18n.Language -> Store -> List (Html Msg)
view endpoint assemblage =
    case assemblage.kind of
        Assemblage.Person ->
            Page.Person.view endpoint assemblage

        Assemblage.Composition ->
            Page.Composition.view endpoint assemblage

        Assemblage.Recording ->
            Page.Performance.view endpoint assemblage

        Assemblage.Generic ->
            Page.Person.view endpoint assemblage
