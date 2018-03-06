module Page.Stats exposing (view)

import Html exposing (Html, text)
import I18n
import Connection.Server as Server


view : Server.Model -> I18n.Language -> List (Html msg)
view server _ =
    []
