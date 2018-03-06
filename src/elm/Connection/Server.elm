module Connection.Server exposing (..)

import Http
import Json.Encode as JE
import Json.Decode as JD
import Jwt
import Store
import Celeste
import LocalStorage


type alias User =
    { username : String
    , jwt : String
    , stats : List String
    , lastfm : Maybe String
    }


type alias Wannabe =
    { username : String
    , password : String
    }


type State
    = NotAuthenticated Wannabe
    | Authenticated User Store.Model


type alias MaybeCmd msg =
    Maybe (Cmd msg)


type alias PreauthMaybeCmd msg =
    String -> String -> MaybeCmd msg


type alias FetchAndHandle msg =
    Celeste.Route -> (Celeste.ResponseResult -> msg) -> PreauthMaybeCmd msg


type alias Model =
    { endpoint : String
    , state : State
    }


initialWannabe : Wannabe
initialWannabe =
    { username = "", password = "" }


initialUser : String -> Maybe String -> Maybe User
initialUser endpoint mToken =
    mToken |> Maybe.andThen (fetchUser endpoint)


initialModel : String -> Maybe String -> Model
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


maybeAuthenticatedFetchAndHandle : (FetchAndHandle msg -> PreauthMaybeCmd msg) -> Model -> MaybeCmd msg
maybeAuthenticatedFetchAndHandle fetchWrapper model =
    case model.state of
        Authenticated { jwt } _ ->
            fetchWrapper fetchAndHandle model.endpoint jwt

        NotAuthenticated _ ->
            Nothing


fetchUser : String -> String -> Maybe User
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


sessionDecoder : JD.Decoder User
sessionDecoder =
    let
        f username jwt =
            User username jwt [] Nothing
    in
        JD.field "session"
            (JD.map2
                f
                (JD.field "username" JD.string)
                (JD.field "jwt" JD.string)
            )


sessionEncoder : Wannabe -> JE.Value
sessionEncoder { username, password } =
    JE.object
        [ ( "session"
          , JE.object
                [ ( "username", JE.string username )
                , ( "password", JE.string password )
                ]
          )
        ]


signIn : String -> Wannabe -> Http.Request User
signIn endpoint =
    Jwt.authenticate (endpoint ++ "/session") sessionDecoder << sessionEncoder
