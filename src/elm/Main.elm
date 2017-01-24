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
import Models.Tag as Tag exposing (Tag)
import Models.File as File exposing (File)
import Components.Bootstrap exposing (horizontalForm, inputFormGroup)
import Task

main : Program Never Model Msg
main =
  Navigation.program processUrl
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

initialModel : Navigation.Location -> Model
initialModel navLocation =
  { routing = Routing.initialModel navLocation
  , session = Session.initialModel
  , flash = Flash.initialModel
  , store = Store.initialModel
  }

init : Navigation.Location -> ( Model, Cmd Msg )
init navLocation =
  ( initialModel navLocation
  , Cmd.none
  )

-- UPDATE

type Msg
  = Noop
  | SignIn
  | SignInSucceed Session.User
  | SignInFail Http.Error
  | SignOut
  | SessionMsg Session.Msg
  | RoutingMsg Routing.Msg

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    Noop ->
      ( model, Cmd.none )

    SignIn ->
      let
        signIn { username, password } =
          let
            url = "http://localhost:4000/api/session"
            sessionObject =
              JE.object
                [ ("username", JE.string username)
                , ("password", JE.string password)
                ]
            wrappedSession = JE.object [ ( "session", sessionObject ) ]
            httpBody = Http.jsonBody wrappedSession
            sessionDecoder =
              JD.map2 Session.User
                (JD.at ["session", "username"] JD.string)
                (JD.at ["session", "jwt"] JD.string)
            fetch = Http.toTask (Http.post url httpBody sessionDecoder)
            process result =
              case result of
                Ok user ->
                  SignInSucceed user
                Err error ->
                  SignInFail error
          in
            Task.attempt process fetch
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
        redirectCmd = Task.perform (RoutingMsg << Routing.SetRoute) <| Task.succeed Routing.Root
      in ( { model | session = new }, redirectCmd )
    SignInFail error ->
      case error of
        Http.BadPayload message response ->
          ( model, Cmd.none )
        Http.BadStatus resp ->
          let (updatedFlash, cmd) = Flash.update (Flash.DeriveFrom resp) model.flash
          in ( { model | flash = updatedFlash }, Cmd.none )
        _ ->
          ( model, Cmd.none )

    SessionMsg msg ->
      let (updatedSession, cmd) = Session.update msg model.session
      in ( { model | session = updatedSession }, Cmd.map SessionMsg cmd )
    RoutingMsg msg ->
      let
        (updatedFlash, flashCmd) = Flash.update Flash.Flush model.flash
        (updatedRouting, subCmd) = Routing.update msg model.routing
        routingCmd = Cmd.map RoutingMsg subCmd
        cmd = Cmd.batch [flashCmd, routingCmd]
      in ( { model | flash = updatedFlash, routing = updatedRouting }, cmd )

-- VIEW

navLink : Routing.Route -> List (Attribute Msg) -> List (Html Msg) -> Html Msg
navLink = Routing.navLink RoutingMsg

template : Model -> List (Html Msg)
template { routing, store, session } =
  case routing.currentRoute of
    Just Routing.Root ->
      [root]
    Just Routing.NewSession ->
      newSessionView session
    Just Routing.Composers ->
      let assemblages = List.filter Assemblage.isComposer store.assemblages
      in assemblagesView assemblages
    Just (Routing.Assemblage id) ->
      case findInList (\a -> a.id == id) store.assemblages of
        Just assemblage ->
          assemblageView assemblage store
        Nothing ->
          [notFound]
    Nothing ->
      [notFound]

root : Html msg
root =
  text "Hi."

notFound : Html msg
notFound =
  div []
    [ h1 [] [ text "Not found" ]
    , p [] [ text "Try again." ]
    ]

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
    |> List.filter (\a -> (foreignKey a) == id && a.kind == assemblyKind)
    |> List.map furtherForeignKey
    |> List.filterMap (findById assemblages)
    |> List.filter (\a -> a.kind == assemblageKind)

enumerateHuman : List (Html Msg) -> List (Html Msg)
enumerateHuman list =
  case list of
    [] ->
      []
    [head] ->
      [head]
    [head1, head2] ->
      [head1, text " and ", head2]
    head :: tail ->
      [head, text ", "] ++ (enumerateHuman tail)

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
    files = List.filterMap (findById store.files) assemblage.fileIds
    compositions =
      assemblagesThroughAssemblies assemblage store .assemblageId .childAssemblageId Assembly.Composed Assemblage.Composition
    reconstructions =
      assemblagesThroughAssemblies assemblage store .assemblageId .childAssemblageId Assembly.Reconstructed Assemblage.Composition
  in
    header
    ++ (fileTable files)
    ++ (assemblageTable "Compositions" compositions)
    ++ (assemblageTable "Reconstructions of other composers' works" reconstructions)

compositionHeader : Assemblage -> Store -> Bool -> List (Html Msg)
compositionHeader assemblage store h1Link =
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

tagsRow : List Tag -> List (Html Msg)
tagsRow tags =
  if List.isEmpty tags then
    []
  else
    [ small []
      (
        text "tags: "
        :: (List.map (\t -> span [ class "label label-default" ] [ text t.name ]) tags)
      )
    ]

compositionView : Assemblage -> Store -> List (Html Msg)
compositionView assemblage store =
  let
    header =
      compositionHeader assemblage store False
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
    header =
      compositions
        |> List.map (\c -> compositionHeader c store True)
        |> List.foldr (++) []
    files = List.filterMap (findById store.files) assemblage.fileIds
  in header ++ (fileTable files)

attrUmbrella : String -> List ( String, String ) -> List (Attribute Msg)
attrUmbrella parent =
  let mapper (key, value) = attribute (parent ++ "-" ++ key) value
  in List.map mapper

data : List ( String, String ) -> List (Attribute Msg)
data = attrUmbrella "data"

aria : List ( String, String ) -> List (Attribute Msg)
aria = attrUmbrella "aria"

view : Model -> Html Msg
view model =
  let
    threeBars = List.repeat 3 (span [ class "icon-bar" ] [])
    navbarHeader =
      div [ class "navbar-header" ]
        [ button
          (
            [ type_ "button"
            , class "navbar-toggle collapsed"
            ]
            ++ (data [("toggle", "collapse")])
            ++ (aria [("expanded", "false")])
          )
          ( span [ class "sr-only" ] [ text "Toggle navigation" ] :: threeBars )
        , navLink Routing.Root [ class "navbar-brand" ] (faText "music" "Celeste")
        ]
    leftNavbar =
      ul [ class "nav navbar-nav" ]
        [ li []
          [ navLink Routing.Composers [] [ text "Composers" ]
          ]
        ]
    sessionNav =
      case model.session.user of
        Just user ->
          li [ class "dropdown" ]
            [ a
              ([ href "#", class "dropdown-toggle", attribute "role" "button" ]
                ++ data [("toggle", "dropdown")]
                ++ aria [("haspopup", "true"), ("expanded", "false")])
              (faText "user-circle-o" user.username)
              , ul [ class "dropdown-menu" ]
              [ li [] [ a [ href "#", onClick SignOut ] (faText "sign-out" "Sign Out") ] ]
            ]
        Nothing ->
          li []
            [ navLink Routing.NewSession [] (faText "sign-in" "Sign In")
            ]
    rightNavbar =
      ul [ class "nav navbar-nav navbar-right" ] [ sessionNav ]
  in
    div []
      [ nav [ class "navbar navbar-default" ]
        [ div [ class "container" ]
          [ navbarHeader
          , div [ class "collapse navbar-collapse" ]
            [ leftNavbar
            , rightNavbar
            ]
          ]
        ]
        , div [ class "container" ]
          (
            (Flash.view model.flash)
            ++ [ main_ [ attribute "role" "main" ] (template model) ]
          )
      ]

-- FUNCTIONS

findInList : (a -> Bool) -> List a -> Maybe a
findInList predicate list =
  case list of
    [] ->
      Nothing
    head :: tail ->
      if predicate head then
        Just head
      else
        findInList predicate tail

findById : List { b | id : a } -> a -> Maybe { b | id : a }
findById list fileId =
  findInList (\f -> f.id == fileId) list

processUrl : Navigation.Location -> Msg
processUrl = RoutingMsg << Routing.VisitLocation
