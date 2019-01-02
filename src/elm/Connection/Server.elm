module Connection.Server exposing (..)

import Http
import Jwt
import Store
import Celeste
import LocalStorage
import Connection.Server.Types exposing (..)
import Connection.Session


type State
    = NotAuthenticated Wannabe
    | Authenticated User Store.Model


type alias MaybeCmd msg =
    Maybe (Cmd msg)


type alias MaybeAuthenticatedCmd msg =
    Endpoint -> String -> MaybeCmd msg


type alias FetchAndHandle msg =
    Celeste.Route -> (Celeste.ResponseResult -> msg) -> MaybeAuthenticatedCmd msg


type alias Model =
    { endpoint : Endpoint
    , state : State
    }


initialWannabe : Wannabe
initialWannabe =
    { username = "", password = "" }


initialUser : Endpoint -> Maybe String -> Maybe User
initialUser endpoint mToken =
    mToken |> Maybe.andThen (fetchUser endpoint)


initialModel : Endpoint -> Maybe String -> Model
initialModel endpoint token =
    let
        state =
            case initialUser endpoint token of
                Just user ->
                    Authenticated user Store.initialModel

                Nothing ->
                    NotAuthenticated initialWannabe
    in
        { endpoint = endpoint, state = state }



-- UPDATE


type Msg
    = UpdateWannabeUsername String
    | UpdateWannabePassword String
    | FlushWannabe
    | SignInSucceed User
    | SignOut
    | StoreRecords Celeste.ResponseTuple


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateWannabeUsername value ->
            case model.state of
                NotAuthenticated wannabe ->
                    ( { model | state = NotAuthenticated { wannabe | username = value } }, Cmd.none )

                Authenticated _ _ ->
                    ( model, Cmd.none )

        UpdateWannabePassword value ->
            case model.state of
                NotAuthenticated wannabe ->
                    ( { model | state = NotAuthenticated { wannabe | password = value } }, Cmd.none )

                Authenticated _ _ ->
                    ( model, Cmd.none )

        FlushWannabe ->
            case model.state of
                NotAuthenticated _ ->
                    ( { model | state = NotAuthenticated initialWannabe }, Cmd.none )

                Authenticated _ _ ->
                    ( model, Cmd.none )

        SignInSucceed user ->
            ( { model | state = Authenticated user Store.initialModel }, LocalStorage.set "token" user.jwt )

        SignOut ->
            ( { model | state = NotAuthenticated initialWannabe }, LocalStorage.remove "token" )

        StoreRecords tuple ->
            case model.state of
                NotAuthenticated _ ->
                    ( model, Cmd.none )

                Authenticated user store ->
                    let
                        ( updatedStore, cmd ) =
                            Store.update tuple store
                    in
                        ( { model | state = Authenticated user updatedStore }, cmd )



-- FUNCTIONS


fetchAndHandle : FetchAndHandle msg
fetchAndHandle celesteRoute resultHandler endpoint jwt =
    let
        decoder =
            Celeste.decoderForRoute celesteRoute

        url =
            Celeste.url endpoint celesteRoute

        get =
            Jwt.get jwt url decoder
    in
        Just (Jwt.send resultHandler get)


maybeAuthenticatedFetchAndHandle : (FetchAndHandle msg -> MaybeAuthenticatedCmd msg) -> Model -> MaybeCmd msg
maybeAuthenticatedFetchAndHandle fetchWrapper model =
    case model.state of
        Authenticated { jwt } _ ->
            fetchWrapper fetchAndHandle model.endpoint jwt

        NotAuthenticated _ ->
            Nothing


fetchUser : Endpoint -> String -> Maybe User
fetchUser endpoint jwt =
    let
        user =
            { username = ""
            , lastfm = Nothing
            , stats = []
            , jwt = jwt
            }
    in
        Just user


signInCmd : Model -> (Result Http.Error User -> msg) -> Cmd msg
signInCmd { state, endpoint } responseToMsg =
    case state of
        Authenticated _ _ ->
            Cmd.none

        NotAuthenticated wannabe ->
            wannabe
                |> signInRequest endpoint
                |> Http.send responseToMsg


signInRequest : Endpoint -> Wannabe -> Http.Request User
signInRequest endpoint wannabe =
    Jwt.authenticate (Celeste.url endpoint Celeste.Session) Connection.Session.decoder (Connection.Session.encoder wannabe)


pingRequest : Endpoint -> Http.Request String
pingRequest endpoint =
    Http.request
        { method = "GET"
        , headers = []
        , url = Celeste.url endpoint Celeste.Root
        , body = Http.emptyBody
        , expect = Http.expectString
        , timeout = Just 2000
        , withCredentials = False
        }
