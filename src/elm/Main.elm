module Main exposing (main)

import Html exposing (Html, Attribute,
                      div, main_, nav, footer,
                      text, a, span, button, h1, h3, h4,
                      p, ul, li,
                      table, thead, tbody, tr, th, td,
                      hr)
import Html.Attributes exposing (class, type_, attribute, href, target, placeholder)
import Html.Events exposing (onClick, onWithOptions)
import Http
import Navigation
import Regex
import Jwt
import Dict
import Json.Decode as JD

import Components.Html exposing (data, aria)
import Components.FontAwesome exposing (fa, faText)
import Components.Flash as Flash
import Components.Bootstrap exposing (horizontalForm, inputFormGroup)
import Components.Player as Player
import Models.Assemblage as Assemblage exposing (Assemblage)
import Models.Assembly as Assembly exposing (Assembly)
import Models.File as File exposing (File)
import Models.Tag as Tag exposing (Tag)
import Routing
import Server
import Celeste
import Views exposing (..)
import Session exposing (Session)
import Store exposing (Store)
import Messages exposing (..)
import I18n exposing (t)

main : Program Flags Model Msg
main =
  Navigation.programWithFlags VisitLocation
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- MODEL

type alias Flags =
  { token : Maybe String
  }

type alias Model =
  { routing : Routing.Model
  , language : I18n.Language
  , flash : Flash.Model
  , session : Session.Model
  , server : Server.Model
  , player : Player.Model
  }

init : Flags -> Navigation.Location -> ( Model, Cmd Msg )
init { token } navLoc =
  let
    serverRoot = navLoc.origin
    preModel =
      { routing = Routing.initialModel
      , language = I18n.English
      , session = Session.initialModel token
      , flash = Flash.initialModel
      , server = Server.initialModel serverRoot
      , player = Player.initialModel
      }
    (model, navCmd) = processLocation navLoc preModel
  in (model, navCmd)

-- UPDATE

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    Noop -> (model, Cmd.none)
    FlashMsg msg ->
      let (updatedFlash, cmd) = Flash.update msg model.flash
      in ( { model | flash = updatedFlash }, Cmd.map FlashMsg cmd )
    SessionMsg msg ->
      let (updatedSession, cmd) = Session.update msg model.session
      in ( { model | session = updatedSession }, Cmd.map SessionMsg cmd )
    PlayerMsg msg ->
      let (updatedPlayer, cmd) = Player.update msg model.player model.server.endpoint
      in ({ model | player = updatedPlayer }, Cmd.map PlayerMsg cmd)
    ServerMsg msg ->
      let (updatedServer, cmd) = Server.update msg model.server
      in ({ model | server = updatedServer }, Cmd.map ServerMsg cmd)
    SetRoute route ->
      ( model, Navigation.newUrl (Routing.routeToString route) )
    VisitLocation navLoc ->
      let
        (model_, fetchCmd) = processLocation navLoc model
        (model__, flashCmd) = update (FlashMsg Flash.Flush) model_
      in (model__, Cmd.batch [fetchCmd, flashCmd])
    SignIn ->
      let
        cmd =
          case model.session of
            Session.Blank wannabe ->
              wannabe
                |> Session.signIn model.server.endpoint
                |> Http.send (\result ->
                  case result of
                    Ok user -> SignInSucceed user
                    Err error -> SignInFail error)
            Session.Present _ ->
              Cmd.none
      in (model, cmd)
    SignInSucceed user ->
      let
        (model_, sessionCmd) = update (SessionMsg (Session.SignInSucceed user)) model
        (model__, redirect) = update (SetRoute Routing.Root) model_
      in (model__, Cmd.batch [sessionCmd, redirect])
    SignInFail error ->
      case error of
        Http.BadStatus resp ->
          (update << FlashMsg << Flash.DeriveFromResponse) resp model
        _ ->
          (model, Cmd.none)

-- SUB

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch [Sub.map PlayerMsg (Player.subscriptions model.player)]

-- VIEW

template : Model -> List (Html Msg)
template { language, routing, session, server } =
  case routing.currentRoute of
    Just Routing.Root ->
      root server.endpoint language
    Just Routing.NewSession ->
      newSessionView session language
    Just Routing.Stats ->
      statsView session
    Just Routing.Composers ->
      case server.state of
        Server.Connected { store } ->
          let
            assemblagesStore = Dict.filter (\_ a -> Assemblage.isComposer a) store.assemblages
            assemblages = (List.sortBy .name << List.map Tuple.second << Dict.toList) assemblagesStore
          in assemblagesView assemblages
        Server.Disconnected ->
          [notFound]
    Just (Routing.Assemblage id _) ->
      case server.state of
        Server.Connected { store } ->
          case Dict.get id store.assemblages of
            Just assemblage ->
              assemblageView language assemblage store
            Nothing ->
              [notFound]
        Server.Disconnected ->
          [notFound]
    Nothing ->
      [notFound]

newSessionView : Session -> I18n.Language -> List (Html Msg)
newSessionView session language =
  case session of
    Session.Blank { username, password } ->
      [ h1 [] (t language I18n.SignIn)
      , horizontalForm SignIn
        [ inputFormGroup "user" "username" "Username" "text" username
          (SessionMsg << Session.UpdateWannabeUsername)
          [ placeholder "seanbooth" ]
        , inputFormGroup "user" "password" "Password" "password" password
          (SessionMsg << Session.UpdateWannabePassword)
          [ placeholder "6IE.CR" ]
        , div [ class "form-group" ]
          [ div [ class "col-lg-10 col-lg-offset-2" ]
            [ button [ type_ "submit", class "btn btn-primary" ] (faText "sign-in" "Sign In") ]
          ]
        ]
      ]
    Session.Present _ ->
      [ h1 [] [ text "You are signed in." ] ]

assemblagesView : List Assemblage -> List (Html Msg)
assemblagesView = flip (::) [] << div [] << List.map assemblageRowView

assemblageLink : Assemblage -> Html Msg
assemblageLink assemblage =
  navLink (Routing.Assemblage assemblage.id (Assemblage.toUrlSlug assemblage)) [] [ text assemblage.name ]

assemblageRowView : Assemblage -> Html Msg
assemblageRowView a = p [] [ assemblageLink a ]

compositionRowView : Assemblage -> Html Msg
compositionRowView = assemblageRowView

wikipediaPath : String -> String
wikipediaPath = (++) "https://en.wikipedia.org/wiki/" << Regex.replace Regex.All (Regex.regex " ") (always "_")

assemblageView : I18n.Language -> Assemblage -> Store -> List (Html Msg)
assemblageView language assemblage =
  case assemblage.kind of
    Assemblage.Person ->
      personView language assemblage
    Assemblage.Composition ->
      compositionView assemblage
    Assemblage.Recording ->
      performanceView assemblage
    Assemblage.General ->
      personView language assemblage

assemblageRow : Assemblage -> Html Msg
assemblageRow a =
  tr [] [ td [] [ assemblageLink a ] ]

assemblageTable : List (Html Msg) -> List Assemblage -> List (Html Msg)
assemblageTable name assemblages =
  if List.isEmpty assemblages then
    []
  else
    [ table [ class "table" ]
      [ thead [] [ tr [] [ th [] name ] ]
      , tbody [] (List.map assemblageRow assemblages)
      ]
    ]

fileRowView : Msg -> File -> Html Msg
fileRowView msg { name } =
  p []
    [ a [ href "", onWithOptions "click" { stopPropagation = True, preventDefault = True } (JD.succeed msg) ]
      [ text name ]
    ]

fileTable : List File -> List (Html Msg)
fileTable files =
  List.map (\file -> fileRowView (PlayerMsg <| Player.Update files file) file) files

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
    |> List.filterMap (flip Dict.get assemblages)
    |> List.filter ((==) assemblageKind << .kind)

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
prependAndEnumerateLinks = flip (<<) (enumerateHuman << List.map assemblageLink) << (::) << text << flip (++) " "

personView : I18n.Language -> Assemblage -> Store -> List (Html Msg)
personView language assemblage store =
  let
    header =
      [ h1 [] [ text assemblage.name ]
      , p [] [ a [ href (wikipediaPath assemblage.name), target "_blank" ] [ text "Wikipedia" ] ]
      ]
    files = List.filterMap (flip Dict.get store.files) assemblage.fileIds
    shuffledPerformances =
      assemblagesThroughAssemblies assemblage store .assemblageId .childAssemblageId Assembly.Performed Assemblage.Recording
    performances =
      List.sortBy .name shuffledPerformances
    shuffledCompositions =
      assemblagesThroughAssemblies assemblage store .assemblageId .childAssemblageId Assembly.Composed Assemblage.Composition
    compositions =
      List.sortBy .name shuffledCompositions
    reconstructions =
      assemblagesThroughAssemblies assemblage store .assemblageId .childAssemblageId Assembly.Reconstructed Assemblage.Composition
  in
    header
    ++ (fileTable files)
    ++ (assemblageTable (t language I18n.Performances) performances)
    ++ (assemblageTable (t language I18n.Compositions) compositions)
    ++ (assemblageTable [ text "Reconstructions of other composers' works" ] reconstructions)

compositionHeader : Store -> Bool -> Assemblage -> ( List (Html Msg), List Tag )
compositionHeader store h1Link assemblage =
  let
    allTags =
      List.filterMap (flip Dict.get store.tags) assemblage.tagIds
    (creationDateTags, tags_) = List.partition ((==) "creation_date" << .key) allTags
    (tonalityTags, tags) = List.partition ((==) "tonality" << .key) tags_
    tonality =
      if List.isEmpty tonalityTags then
        []
      else
        [ text " "
        , span [ class "text-muted" ] (text "in " :: enumerateHuman (List.map (text << .value) tonalityTags))
        ]
    name =
      if h1Link then
        assemblageLink assemblage
      else
        (text << .name) assemblage
    postName = tonality
    fullName = name :: postName
    nameHeader = h1 [] fullName
    composers =
      assemblagesThroughAssemblies assemblage store .childAssemblageId .assemblageId Assembly.Composed Assemblage.Person
    creationDate =
      if List.isEmpty creationDateTags then
        []
      else
        text " in " :: enumerateHuman (List.map (text << .value) creationDateTags)
    composedByHeader =
      if List.isEmpty composers then
        []
      else
        [ h3 []
          ([ text "composed " ]
          ++ prependAndEnumerateLinks "by" composers
          ++ creationDate
          )
        ]
    reconstructors =
      assemblagesThroughAssemblies assemblage store .childAssemblageId .assemblageId Assembly.Reconstructed Assemblage.Person
    reconstructedByHeader =
      if List.isEmpty reconstructors then
        []
      else
        [ h4 []
          ([ text "reconstructed " ]
          ++ prependAndEnumerateLinks "by" reconstructors
          )
        ]
  in ([nameHeader] ++ composedByHeader ++ reconstructedByHeader, tags)

compositionView : Assemblage -> Store -> List (Html Msg)
compositionView assemblage store =
  let
    (header, tags) = compositionHeader store False assemblage
    recordings =
      assemblagesThroughAssemblies assemblage store .assemblageId .childAssemblageId Assembly.Recorded Assemblage.Recording
  in List.concat [header, tagsRow tags, assemblageTable [text "Performances"] recordings, fileTable []]

performanceView : Assemblage -> Store -> List (Html Msg)
performanceView assemblage store =
  let
    compositions =
      assemblagesThroughAssemblies assemblage store .childAssemblageId .assemblageId Assembly.Recorded Assemblage.Composition
    inheritedHeader =
      compositions
        |> List.map (Tuple.first << compositionHeader store True)
        |> List.foldr (++) []
    performers =
      assemblagesThroughAssemblies assemblage store .childAssemblageId .assemblageId Assembly.Performed Assemblage.Person
    mainPerformanceHeader =
      if List.isEmpty performers then
        []
      else
        [h4 [] ([ text "performed " ] ++ prependAndEnumerateLinks "by" performers)]
    performanceHeader =
      mainPerformanceHeader ++
      [ h4 [] [ text assemblage.name ]
      ]
    files =
      assemblage.fileIds
        |> List.filterMap (flip Dict.get store.files)
        |> List.sortBy .name
  in inheritedHeader ++ performanceHeader ++ (fileTable files)

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
        , navLink Routing.Root [ class "navbar-brand" ] [ text "Celeste" ]
        ]
    authenticatedItems =
      case model.session of
        Session.Present _ ->
          [ li [] [ navLink Routing.Composers [] (t model.language I18n.Composers) ]
          ]
        Session.Blank _ ->
          []
    leftNav = [ ul [ class "nav navbar-nav" ] authenticatedItems ]
    sessionNav =
      case model.session of
        Session.Present { username } ->
          li [ class "dropdown" ]
            [ a
              ([ href "#", class "dropdown-toggle", attribute "role" "button" ]
              ++ data [("toggle", "dropdown")]
              ++ aria [("haspopup", "true"), ("expanded", "false")]
              )
              (faText "user-circle-o" username)
            , ul [ class "dropdown-menu" ]
              [ li []
                [ navLink Routing.Stats []
                  (fa "bar-chart" :: t model.language I18n.Stats)
                ]
              , li [ class "divider", attribute "role" "separator" ] []
              , li []
                [ a [ href "#", onClick (SessionMsg Session.SignOut) ]
                  (fa "sign-out" :: t model.language I18n.SignOut)
                ]
              ]
            ]
        Session.Blank _ ->
          li []
            [ navLink Routing.NewSession [] (faText "sign-in" "Sign In")
            ]
    rightNav =
      [ ul [ class "nav navbar-nav navbar-right" ] [ sessionNav ] ]
    navbar =
      nav [ class "navbar navbar-default" ]
        [ div [ class "container" ]
          [ navbarHeader
          , div [ class "collapse navbar-collapse" ] (leftNav ++ rightNav)
          ]
        ]
    mainContainer =
      div [ class "container" ]
        ( Flash.view model.flash
        ++ [ main_ [ attribute "role" "main" ] (template model) ]
        )
    githubLink rel =
      [fa "github", a [href ("https://github.com/" ++ rel), target "_blank"] [text rel]]
    footer_ =
      footer [ class "container" ]
        [ hr [] []
        , p [] ([text "Powered by "] ++ githubLink "lacrosse/scriabin" ++ [text " and "] ++ githubLink "lacrosse/celeste")
        ]
    player = List.map (Html.map PlayerMsg) (Player.view model.player)
  in div [] (navbar :: mainContainer :: footer_ :: player)

-- FUNCTIONS

fetch : String -> Celeste.Route -> String -> Cmd Msg
fetch =
  Store.fetch <| \response ->
    case response of
      Ok value ->
        (ServerMsg << Server.StoreRecords << Celeste.responseToTuple) value
      Err (Jwt.HttpError (Http.BadPayload err _)) ->
        (FlashMsg << Flash.DeriveFromString) err
      _ ->
        Noop

processLocation : Navigation.Location -> Model -> ( Model, Cmd Msg )
processLocation navLoc model =
  let
    route = Routing.locationToRoute navLoc
    newModel = { model | routing = { currentRoute = route } }
    fetchCmd =
      case route of
        Just (Routing.Assemblage id _) ->
          (Session.authorize model.session Cmd.none << fetch model.server.endpoint) (Celeste.Assemblage id)
        Just Routing.Composers ->
          (Session.authorize model.session Cmd.none << fetch model.server.endpoint) Celeste.Composers
        _ ->
          Cmd.none
  in ( newModel, fetchCmd )
