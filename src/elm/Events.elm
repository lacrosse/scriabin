module Events exposing (..)

import Html
import Html.Events exposing (on)
import DOM
import Json.Decode as JD


onMouseUpRelativeWidth : (Float -> msg) -> Html.Attribute msg
onMouseUpRelativeWidth eventMsg =
    let
        progressTargetDecoder =
            JD.map3
                (\clientX clientY target -> (toFloat clientX - target.left) / target.width)
                (JD.field "clientX" JD.int)
                (JD.field "clientY" JD.int)
                (JD.field "currentTarget" DOM.boundingClientRect)
    in
        on "mouseup" (JD.map eventMsg progressTargetDecoder)
