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
    { currentRoute = Nothing
    , transitioning = False
    }



-- UPDATE


type Msg
    = StartTransition
    | FinishTransition (Maybe Route)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        StartTransition ->
            ( { model | transitioning = True }, Cmd.none )

        FinishTransition mRoute ->
            case mRoute of
                Just route ->
                    ( { model | currentRoute = Just route, transitioning = False }, Cmd.none )

                Nothing ->
                    ( { model | transitioning = False }, Cmd.none )



-- FUNCTIONS


basedInt : Parser (Int -> b) b
basedInt =
    let
        firstInStringToInt str =
            str
                |> Regex.split (Regex.AtMost 1) (Regex.regex "-")
                |> List.head
                |> Maybe.withDefault str
                |> String.toInt
    in
        custom "BASED_INT" firstInStringToInt


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
    routingTable
        |> oneOf
        |> parseHash


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
