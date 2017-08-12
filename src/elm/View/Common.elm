module View.Common exposing (..)

import Html
    exposing
        ( Html
        , Attribute
        , span
        , text
        , small
        , h1
        , p
        , div
        , a
        , table
        , tr
        , td
        )
import Html.Attributes exposing (class, href, target)
import Html.Events exposing (onWithOptions)
import Data.Assemblage as Assemblage exposing (Assemblage)
import Data.Tag exposing (Tag)
import Data.File exposing (File)
import Messages exposing (Msg)
import Json.Decode as JD
import Routing
import I18n exposing (..)
import Components.FontAwesome exposing (fa)
import Components.Player as Player


-- VIEW


navLink : Routing.Route -> List (Attribute Messages.Msg) -> List (Html Messages.Msg) -> Html Messages.Msg
navLink route =
    let
        string =
            Routing.routeToString route

        options =
            { stopPropagation = False, preventDefault = True }

        on =
            onWithOptions "click" options (JD.succeed << Messages.SetRoute <| route)
    in
        (a << (::) (href string) << (::) on)


tagLabel : Tag -> Html msg
tagLabel { value } =
    span [ class "label label-default" ] [ text value ]


tagsRow : List Tag -> List (Html msg)
tagsRow tags =
    if List.isEmpty tags then
        []
    else
        [ small [] (text "tags: " :: List.map tagLabel tags) ]


threeBars : List (Html msg)
threeBars =
    List.repeat 3 (span [ class "icon-bar" ] [])


githubLink : String -> String -> List (Html msg)
githubLink user repo =
    [ fa "github", a [ href ("https://github.com/" ++ user ++ "/" ++ repo), target "_blank" ] [ text repo ] ]


enumerateHuman : List (Html msg) -> List (Html msg)
enumerateHuman list =
    case list of
        [] ->
            []

        [ head ] ->
            [ head ]

        [ head1, head2 ] ->
            [ head1, text " and ", head2 ]

        head :: tail ->
            [ head, text ", " ] ++ enumerateHuman tail


prependAndEnumerateLinks : String -> List Assemblage -> List (Html Msg)
prependAndEnumerateLinks string assemblages =
    text (string ++ " ") :: enumerateHuman (List.map assemblageLink assemblages)


assemblageLink : Assemblage -> Html Msg
assemblageLink assemblage =
    navLink (Routing.Assemblage assemblage.id (Assemblage.toUrlSlug assemblage)) [] [ text assemblage.name ]


fileTable : List File -> List (Html Msg)
fileTable files =
    let
        playMsg =
            Messages.PlayerMsg << Player.Update files

        queueMsg =
            Messages.PlayerMsg << Player.Append << List.singleton

        rows =
            List.map (\file -> fileRow ( playMsg file, queueMsg file ) file) files
    in
        [ table [ class "table table-audio-list" ] rows ]


fileRow : ( Msg, Msg ) -> File -> Html Msg
fileRow ( playMsg, queueMsg ) { name } =
    let
        linkOptions msg =
            [ href ""
            , onWithOptions "click"
                { stopPropagation = True
                , preventDefault = True
                }
                msg
            ]
    in
        tr []
            [ td []
                [ a (linkOptions (JD.succeed playMsg)) [ fa "play" ]
                , a (linkOptions (JD.succeed queueMsg)) [ fa "list" ]
                ]
            , td []
                [ text name
                ]
            ]
