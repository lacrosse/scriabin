module Page.Person exposing (view)

import I18n exposing (t)
import Html exposing (Html, h1, text)
import Dict
import Store exposing (Store, assemblagesThroughAssemblies)
import Messages exposing (Msg)
import Data.Assemblage as Assemblage exposing (Assemblage)
import Data.Assembly as Assembly exposing (Assembly)
import View.Common exposing (fileTable)
import View.Assemblage


view : String -> Assemblage -> I18n.Language -> Store -> List (Html Msg)
view endpoint assemblage language store =
    let
        header =
            [ h1 [] [ text assemblage.name ] ]

        files =
            List.filterMap (flip Dict.get store.files) assemblage.fileIds

        performances_ =
            assemblagesThroughAssemblies
                store
                assemblage
                .assemblageId
                .childAssemblageId
                Assembly.Performed

        compositions_ =
            assemblagesThroughAssemblies
                store
                assemblage
                .assemblageId
                .childAssemblageId
                Assembly.Composed

        performances =
            List.sortBy .name performances_

        compositions =
            List.sortBy .name compositions_

        reconstructions =
            assemblagesThroughAssemblies
                store
                assemblage
                .assemblageId
                .childAssemblageId
                Assembly.Reconstructed
    in
        header
            ++ (fileTable endpoint files)
            ++ (View.Assemblage.table (t language I18n.Performances) performances)
            ++ (View.Assemblage.table (t language I18n.Compositions) compositions)
            ++ (View.Assemblage.table [ text "Reconstructions of other composers' works" ] reconstructions)
