module Components.Bootstrap exposing (..)

import Html exposing (Html, Attribute, div, label, input, text, form)
import Html.Attributes exposing (class, id, name, type_, for, value)
import Html.Events exposing (onInput, onSubmit)

inputFormGroup : String -> String -> String -> String -> String -> (String -> msg) -> List (Attribute msg) -> Html msg
inputFormGroup resource attribute humanAttr attrType attrValue msgWrapper inputAttrs =
  let
    inputId = resource ++ "_" ++ attribute
    inputName = resource ++ "[" ++ attribute ++ "]"
  in
    div [ class "form-group" ]
      [ label ([ for inputId, class "col-lg-2 control-label" ]) [ text humanAttr ]
      , div [ class "col-lg-10" ]
        [ input
          (
            [ id inputId
            , name inputName
            , value attrValue
            , onInput msgWrapper
            , class "form-control"
            , type_ attrType
            ]
            ++ inputAttrs
          )
          []
        ]
      ]

horizontalForm : msg -> List (Html msg) -> Html msg
horizontalForm msg =
  form [ class "form-horizontal", onSubmit msg ]
