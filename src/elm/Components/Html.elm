module Components.Html exposing (data, aria)

import Html exposing (Attribute)
import Html.Attributes exposing (attribute)

attrUmbrella : String -> List ( String, String ) -> List (Attribute msg)
attrUmbrella parent =
  let mapper (key, value) = attribute (parent ++ "-" ++ key) value
  in List.map mapper

data : List ( String, String ) -> List (Attribute msg)
data = attrUmbrella "data"

aria : List ( String, String ) -> List (Attribute msg)
aria = attrUmbrella "aria"
