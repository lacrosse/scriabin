module Routing exposing (..)

import Navigation exposing (Location)
import UrlParser exposing (Parser, parseHash, oneOf, top, map, s, (</>), int, custom)
import Regex


-- MODEL


type Route
    = Root
    | NewSession
    | Stats
    | Composers
    | Assemblage Int String


type alias Model =
    { currentRoute : Maybe Route
    , transitioning : Bool
    }


initialModel : Model
initialModel =
    { currentRoute = Nothing, transitioning = False }



-- UPDATE


type Msg
    = FinishTransition (Maybe Route)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FinishTransition mRoute ->
            case mRoute of
                Just route ->
                    ( { model
                        | currentRoute = Just route
                        , transitioning = False
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( { model | transitioning = False }, Cmd.none )



-- FUNCTIONS


basedInt : Parser (Int -> b) b
basedInt =
    custom "BASED_INT" <|
        \s ->
            (String.toInt << Maybe.withDefault s << List.head << Regex.split (Regex.AtMost 1) (Regex.regex "-")) s


routingTable : List (Parser (Route -> c) c)
routingTable =
    [ map (flip Assemblage "") (s "assemblages" </> basedInt)
    , map NewSession (s "sign-in")
    , map Stats (s "stats")
    , map Composers (s "composers")
    , map Root (top)
    ]


locationToRoute : Location -> Maybe Route
locationToRoute =
    parseHash (oneOf routingTable)


routeToString : Route -> String
routeToString route =
    case route of
        Root ->
            "#/"

        NewSession ->
            "#/sign-in"

        Stats ->
            "#/stats"

        Composers ->
            "#/composers"

        Assemblage id string ->
            "#/assemblages/" ++ toString id ++ string
