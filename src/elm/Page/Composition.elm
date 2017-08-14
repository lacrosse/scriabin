module Page.Composition exposing (..)

import Html exposing (Html, text)
import View.Assemblage
import View.Composition.Header
import View.Common
import Store exposing (Store, assemblagesThroughAssemblies)
import Data.Assemblage as Assemblage exposing (Assemblage)
import Data.Assembly as Assembly
import Messages exposing (Msg)
import I18n exposing (t)


view : Assemblage -> I18n.Language -> Store -> List (Html Msg)
view assemblage language store =
    let
        ( header, tags ) =
            View.Composition.Header.view store False assemblage

        recordings =
            assemblagesThroughAssemblies
                store
                assemblage
                .assemblageId
                .childAssemblageId
                Assembly.Recorded
                Assemblage.Recording
    in
        header
            ++ View.Common.tagsRow tags
            ++ View.Assemblage.table (t language I18n.Performances) recordings
            ++ View.Common.fileTable []
