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
        , thead
        , tbody
        , tr
        , th
        , td
        )
import Html.Attributes
    exposing
        ( class
        , href
        , target
        , colspan
        , href
        )
import Html.Events exposing (onWithOptions)
import Data.Assemblage as Assemblage exposing (Assemblage)
import Data.Tag exposing (Tag)
import Data.File exposing (File)
import Messages exposing (Msg)
import Json.Decode as JD
import Routing
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

        [ hd ] ->
            [ hd ]

        [ hd1, hd2 ] ->
            [ hd1, text " and ", hd2 ]

        hd :: tl ->
            hd :: text ", " :: enumerateHuman tl


enumerateLinks : List Assemblage -> List (Html Msg)
enumerateLinks assemblages =
    enumerateHuman (List.map assemblageLink assemblages)


assemblageLink : Assemblage -> Html Msg
assemblageLink assemblage =
    navLink (Routing.Assemblage assemblage.id (Assemblage.toUrlSlug assemblage)) [] [ text assemblage.name ]


fileTable : String -> List File -> List (Html Msg)
fileTable endpoint files =
    if List.isEmpty files then
        []
    else
        files
            |> List.map (\file -> fileRow endpoint files file)
            |> (\rows ->
                    table [ class "table table-audio-list" ]
                        [ thead [] [ tr [] [ th [ colspan 2 ] [ text "Files" ] ] ]
                        , tbody [] rows
                        ]
               )
            |> List.singleton


fileRow : String -> List File -> File -> Html Msg
fileRow endpoint files file =
    if file.mime == "audio/mpeg" then
        audioFileRow endpoint files file
    else if file.mime == "image/jpeg" then
        imageFileRow endpoint file
    else
        genericFileRow endpoint file


genericFileRow : String -> File -> Html Msg
genericFileRow endpoint file =
    tr []
        [ td [] []
        , td [] [ text file.name ]
        ]


imageFileRow : String -> File -> Html Msg
imageFileRow endpoint file =
    tr []
        [ td [] [ a [ href (Data.File.url endpoint file), target "_blank" ] [ fa "eye" ] ]
        , td [] [ text file.name ]
        ]


audioFileRow : String -> List File -> File -> Html Msg
audioFileRow endpoint files file =
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
                [ a (linkOptions (JD.succeed << Messages.PlayerMsg << Player.UpdateSplitFiles files <| file)) [ fa "play" ]
                , a (linkOptions (JD.succeed << Messages.PlayerMsg << Player.Append <| [ file ])) [ fa "list" ]
                ]
            , td [] [ text file.name ]
            ]
