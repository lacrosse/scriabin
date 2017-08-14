module Page.Assemblage exposing (..)

import Html exposing (Html)
import I18n
import Messages exposing (Msg)
import Data.Assemblage as Assemblage exposing (Assemblage)
import Page.Person
import Page.Composition
import Page.Performance
import Store exposing (Store)


view : Assemblage -> I18n.Language -> Store -> List (Html Msg)
view assemblage =
    case assemblage.kind of
        Just (Assemblage.Person) ->
            Page.Person.view assemblage

        Just (Assemblage.Composition) ->
            Page.Composition.view assemblage

        Just (Assemblage.Recording) ->
            Page.Performance.view assemblage

        Nothing ->
            Page.Person.view assemblage
