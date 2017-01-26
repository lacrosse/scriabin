module Main exposing (..)

import Html exposing (Html, Attribute,
                      div, main_, nav,
                      text, a, span, button, h1, h3, h4,
                      p, i, small, ul, li,
                      table, thead, tbody, tr, th, td,
                      form, input, label)
import Html.Attributes exposing (class, id, type_, attribute, href, target, placeholder)
import Html.Events exposing (onClick, onWithOptions, onSubmit)
import Http
import Components.FontAwesome exposing (faText)
import Components.Html exposing (data, aria)
import Views exposing (..)
import Session exposing (Session)
import Components.Flash as Flash
import Navigation
import Routing
import Regex
import Json.Decode as JD
import Json.Encode as JE
import Store exposing (Store)
import Models.Assemblage as Assemblage exposing (Assemblage)
import Models.Assembly as Assembly exposing (Assembly)
import Models.File as File exposing (File)
import Components.Bootstrap exposing (horizontalForm, inputFormGroup)
import Task
import Jwt
import Dict
import Celeste exposing (apiUrl)
import Messages exposing (..)

main : Program Never Model Msg
main =
  Navigation.program VisitLocation
    { init = init
    , view = view
    , update = update
    , subscriptions = always Sub.none
    }

-- MODEL

type alias Model =
  { routing : Routing.Model
  , flash : Flash.Model
  , session : Session.Model
  , store : Store.Model
  }

init : Navigation.Location -> ( Model, Cmd Msg )
init navLoc =
  let
    preModel =
      { routing = Routing.initialModel
      , session = Session.initialModel
      , flash = Flash.initialModel
      , store = Store.initialModel
      }
  in processLocation navLoc preModel

-- UPDATE

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    Noop ->
      ( model, Cmd.none )

    SignIn ->
      let
        signIn { username, password } =
          let
            url = apiUrl "/session"
            sessionObject =
              JE.object
                [ ("username", JE.string username)
                , ("password", JE.string password)
                ]
            wrappedSession = JE.object [ ( "session", sessionObject ) ]
            decodeAttr str = JD.at ["session", str] JD.string
            sessionDecoder = JD.map2 Session.User (decodeAttr "username") (decodeAttr "jwt")
            fetch = Jwt.authenticate url sessionDecoder wrappedSession
            process result =
              case result of
                Ok user -> SignInSucceed user
                Err error -> SignInFail error
          in Http.send process fetch
      in ( model, signIn model.session.wannabe )
    SignOut ->
      let
        old = model.session
        new = { old | user = Nothing }
      in ( { model | session = new }, Cmd.none )
    SignInSucceed user ->
      let
        old = model.session
        new = { old | user = Just user }
        redirectCmd = Task.perform SetRoute <| Task.succeed Routing.Root
      in ( { model | session = new }, redirectCmd )
    SignInFail error ->
      case error of
        Http.BadPayload message response ->
          ( model, Cmd.none )
        Http.BadStatus resp ->
          update (FlashMsg (Flash.DeriveFromResponse resp)) model
        _ ->
          ( model, Cmd.none )

    FlashMsg msg ->
      let (updatedFlash, cmd) = Flash.update msg model.flash
      in ( { model | flash = updatedFlash }, Cmd.map FlashMsg cmd )
    SessionMsg msg ->
      let (updatedSession, cmd) = Session.update msg model.session
      in ( { model | session = updatedSession }, Cmd.map SessionMsg cmd )

    SetRoute route ->
      ( model, Navigation.newUrl (Routing.routeToString route) )
    VisitLocation navLoc ->
      let
        (newModel, fetchCmd) = processLocation navLoc model
        (updatedFlash, flashCmd) = Flash.update Flash.Flush newModel.flash
        cmd = Cmd.batch [fetchCmd, flashCmd]
      in ( { newModel | flash = updatedFlash }, cmd )
    StoreRecords (assemblages, assemblies, files) ->
      let
        old = model.store
        dictifyById = Dict.fromList << List.map (\a -> (a.id, a))
        dictifyByComposite = Dict.fromList << List.map (\a -> ((a.assemblageId, a.childAssemblageId), a))
        assemblagesDict = dictifyById assemblages
        assembliesDict = dictifyByComposite assemblies
        filesDict = dictifyById files
        newAssemblages = Dict.union assemblagesDict old.assemblages
        newAssemblies = Dict.union assembliesDict old.assemblies
        newFiles = Dict.union filesDict old.files
        new = { old | assemblages = newAssemblages, assemblies = newAssemblies, files = newFiles }
      in ( { model | store = new }, Cmd.none )

-- VIEW

template : Model -> List (Html Msg)
template { routing, store, session } =
  case routing.currentRoute of
    Just Routing.Root ->
      [root]
    Just Routing.NewSession ->
      newSessionView session
    Just Routing.Composers ->
      let
        assemblagesStore = Dict.filter (\_ a -> Assemblage.isComposer a) store.assemblages
        assemblages = ((List.sortBy .name) << List.map Tuple.second << Dict.toList) assemblagesStore
      in assemblagesView assemblages
    Just (Routing.Assemblage id) ->
      case Dict.get id store.assemblages of
        Just assemblage ->
          assemblageView assemblage store
        Nothing ->
          [notFound]
    Nothing ->
      [notFound]

newSessionView : Session -> List (Html Msg)
newSessionView model =
  let
    form =
      [ h1 [] [ text "Sign In" ]
      , horizontalForm SignIn
        [ inputFormGroup
          "user"
          "username"
          "Username"
          "text"
          model.wannabe.username
          (SessionMsg << Session.UpdateWannabeUsername)
          [ placeholder "seanbooth" ]
        , inputFormGroup
          "user"
          "password"
          "Password"
          "password"
          model.wannabe.password
          (SessionMsg << Session.UpdateWannabePassword)
          [ placeholder "6IE.CR" ]
        , div [ class "form-group" ]
          [ div [ class "col-lg-10 col-lg-offset-2" ]
            [ button [ type_ "submit", class "btn btn-primary" ] (faText "sign-in" "Sign In") ]
          ]
        ]
      ]
  in form

assemblagesView : List Assemblage -> List (Html Msg)
assemblagesView list =
  [div [] (List.map assemblageRowView list)]

assemblageLink : Assemblage -> Html Msg
assemblageLink { id, name } =
  navLink (Routing.Assemblage id) [] [ text name ]

assemblageRowView : Assemblage -> Html Msg
assemblageRowView a =
  p [] [ assemblageLink a ]

wikipediaPath : String -> String
wikipediaPath name =
  let
    base = "https://en.wikipedia.org/wiki/"
    wikify = Regex.replace Regex.All (Regex.regex " ") (always "_")
    articleName = wikify name
  in base ++ articleName

fileRowView : File -> Html Msg
fileRowView { name } =
  p [] [ text name ]

compositionRowView : Assemblage -> Html Msg
compositionRowView a =
  p [] [ assemblageLink a ]

assemblageView : Assemblage -> Store -> List (Html Msg)
assemblageView assemblage store =
  case assemblage.kind of
    Assemblage.Person ->
      personView assemblage store
    Assemblage.Composition ->
      compositionView assemblage store
    Assemblage.Recording ->
      recordingView assemblage store
    Assemblage.General ->
      personView assemblage store

assemblageRow : Assemblage -> Html Msg
assemblageRow a =
  tr []
    [ td []
      [ assemblageLink a ]
    ]

assemblageTable : String -> List Assemblage -> List (Html Msg)
assemblageTable name assemblages =
  if List.isEmpty assemblages then
    []
  else
    [ table [ class "table" ]
      [ thead []
        [ tr []
          [ th []
            [ text name ]
          ]
        ]
      , tbody [] (List.map assemblageRow assemblages)
      ]
    ]

fileTable : List File -> List (Html Msg)
fileTable =
  List.map fileRowView

assemblagesThroughAssemblies
  : Assemblage
  -> Store
  -> (Assembly -> Int)
  -> (Assembly -> Int)
  -> Assembly.Kind
  -> Assemblage.Kind
  -> List Assemblage
assemblagesThroughAssemblies { id } { assemblies, assemblages } foreignKey furtherForeignKey assemblyKind assemblageKind =
  assemblies
    |> Dict.filter (\_ a -> (foreignKey a) == id && (.kind a) == assemblyKind)
    |> Dict.values
    |> List.map furtherForeignKey
    |> List.filterMap (\id -> Dict.get id assemblages)
    |> List.filter (\a -> (.kind a) == assemblageKind)

enumerateHuman : List (Html msg) -> List (Html msg)
enumerateHuman list =
  case list of
    [] ->
      []
    [head] ->
      [head]
    [head1, head2] ->
      [head1, text " and ", head2]
    head :: tail ->
      [head, text ", "] ++ enumerateHuman tail

prependAndEnumerateLinks : String -> List Assemblage -> List (Html Msg)
prependAndEnumerateLinks str =
  (::) (text (str ++ " ")) << enumerateHuman << (List.map assemblageLink)

composedBy : List Assemblage -> List (Html Msg)
composedBy = prependAndEnumerateLinks "composed by"

reconstructedBy : List Assemblage -> List (Html Msg)
reconstructedBy = prependAndEnumerateLinks "reconstructed by"

personView : Assemblage -> Store -> List (Html Msg)
personView assemblage store =
  let
    header =
      [ h1 [] [ text assemblage.name ]
      , p [] [ a [ href (wikipediaPath assemblage.name), target "_blank" ] [ text "Wikipedia" ] ]
      ]
    files = List.filterMap (\id -> Dict.get id store.files) assemblage.fileIds
    compositions =
      assemblagesThroughAssemblies assemblage store .assemblageId .childAssemblageId Assembly.Composed Assemblage.Composition
    reconstructions =
      assemblagesThroughAssemblies assemblage store .assemblageId .childAssemblageId Assembly.Reconstructed Assemblage.Composition
  in
    header
    ++ (fileTable files)
    ++ (assemblageTable "Compositions" compositions)
    ++ (assemblageTable "Reconstructions of other composers' works" reconstructions)

compositionHeader : Store -> Bool -> Assemblage -> List (Html Msg)
compositionHeader store h1Link assemblage =
  let
    h1Contents =
      if h1Link then
        [ assemblageLink assemblage ]
      else
        [ text (Assemblage.fullName assemblage) ]
    nameHeader = h1 [] h1Contents
    composers =
      assemblagesThroughAssemblies assemblage store .childAssemblageId .assemblageId Assembly.Composed Assemblage.Person
    composedByHeader =
      if List.isEmpty composers then
        []
      else
        [h3 [] (composedBy composers)]
    reconstructors =
      assemblagesThroughAssemblies assemblage store .childAssemblageId .assemblageId Assembly.Reconstructed Assemblage.Person
    reconstructedByHeader =
      if List.isEmpty reconstructors then
        []
      else
        [h4 [] (reconstructedBy reconstructors)]
  in [nameHeader] ++ composedByHeader ++ reconstructedByHeader

compositionView : Assemblage -> Store -> List (Html Msg)
compositionView assemblage store =
  let
    header =
      compositionHeader store False assemblage
    tags = []
    recordings =
      assemblagesThroughAssemblies assemblage store .assemblageId .childAssemblageId Assembly.Recorded Assemblage.Recording
    compositionFiles = []
  in
    header
    ++ (tagsRow tags)
    ++ (assemblageTable "Recordings" recordings)
    ++ (fileTable compositionFiles)

recordingView : Assemblage -> Store -> List (Html Msg)
recordingView assemblage store =
  let
    compositions =
      assemblagesThroughAssemblies assemblage store .childAssemblageId .assemblageId Assembly.Recorded Assemblage.Composition
    inheritedHeader =
      compositions
        |> List.map (compositionHeader store True)
        |> List.foldr (++) []
    recordingHeader =
      [ h4 [] [ text ("recording: " ++ assemblage.name) ] ]
    files = List.filterMap (\id -> Dict.get id store.files) assemblage.fileIds
  in inheritedHeader ++ recordingHeader ++ (fileTable files)

view : Model -> Html Msg
view model =
  let
    threeBars = List.repeat 3 (span [ class "icon-bar" ] [])
    navbarHeader =
      div [ class "navbar-header" ]
        [ button
          ( [ type_ "button"
            , class "navbar-toggle collapsed"
            ]
          ++ (data [("toggle", "collapse")])
          ++ (aria [("expanded", "false")])
          )
          ( span [ class "sr-only" ] [ text "Toggle navigation" ] :: threeBars )
        , navLink Routing.Root [ class "navbar-brand" ] (faText "music" "Celeste")
        ]
    leftNavbar =
      case model.session.user of
        Just _ ->
          [ ul [ class "nav navbar-nav" ]
            [ li [] [ navLink Routing.Composers [] [ text "Composers" ] ]
            ]
          ]
        Nothing ->
          []
    sessionNav =
      case model.session.user of
        Just user ->
          li [ class "dropdown" ]
            [ a
              ([ href "#", class "dropdown-toggle", attribute "role" "button" ]
              ++ data [("toggle", "dropdown")]
              ++ aria [("haspopup", "true"), ("expanded", "false")]
              )
              (faText "user-circle-o" user.username)
            , ul [ class "dropdown-menu" ]
              [ li [] [ a [ href "#", onClick SignOut ] (faText "sign-out" "Sign Out") ] ]
            ]
        Nothing ->
          li []
            [ navLink Routing.NewSession [] (faText "sign-in" "Sign In")
            ]
    rightNavbar =
      [ ul [ class "nav navbar-nav navbar-right" ] [ sessionNav ] ]
  in
    div []
      [ nav [ class "navbar navbar-default" ]
        [ div [ class "container" ]
          [ navbarHeader
          , div [ class "collapse navbar-collapse" ] (leftNavbar ++ rightNavbar)
          ]
        ]
        , div [ class "container" ]
          (
            (Flash.view model.flash)
            ++ [ main_ [ attribute "role" "main" ] (template model) ]
          )
      ]

-- FUNCTIONS

handleCelesteResponse : Result Jwt.JwtError Celeste.Response -> Msg
handleCelesteResponse response =
  case response of
    Ok cResp ->
      StoreRecords (Celeste.responseToTuple cResp)
    Err (Jwt.HttpError (Http.BadPayload err _)) ->
      (FlashMsg << Flash.DeriveFromString) err
    _ ->
      Noop

fetch : Celeste.Route -> String -> Cmd Msg
fetch = Store.fetch handleCelesteResponse

processLocation : Navigation.Location -> Model -> ( Model, Cmd Msg )
processLocation navLoc model =
  let
    route = Routing.locationToRoute navLoc
    newModel = { model | routing = { currentRoute = route } }
    authorized f =
      case newModel.session.user of
        Just { jwt } -> f jwt
        Nothing -> Cmd.none
    fetchCmd =
      case route of
        Just (Routing.Assemblage id) ->
          (authorized << fetch) (Celeste.Assemblage id)
        Just Routing.Composers ->
          (authorized << fetch) Celeste.Composers
        _ -> Cmd.none
  in ( newModel, fetchCmd )
