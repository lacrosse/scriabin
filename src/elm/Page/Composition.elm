module Page.Composition exposing (..)

import Html exposing (Html, text)
import Data.Assemblage as Assemblage exposing (Assemblage)
import Data.Tag as Tag exposing (Tag)
import View.Assemblage
import View.Composition.Header
import View.Common
import Messages exposing (Msg)
import I18n exposing (t)


view :
    List Assemblage
    -> List Assemblage
    -> List Tag
    -> List Assemblage
    -> Assemblage
    -> I18n.Language
    -> String
    -> List (Html Msg)
view composers reconstructors tags embodiments composition language endpoint =
    let
        ( header, tags_ ) =
            View.Composition.Header.view False composers reconstructors tags composition
    in
        header
            ++ View.Common.tagsRow tags_
            ++ View.Assemblage.table (t language I18n.Performances) embodiments
            ++ View.Common.fileTable endpoint []
