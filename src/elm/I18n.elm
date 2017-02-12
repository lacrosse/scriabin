module I18n exposing (..)

import Html exposing (Html, text, code)

type Language
  = English
  | Russian

type Sentence
  = Welcome
  | YouAreConnectedTo String
  | NoReasonToPanic
  | Composers
  | Compositions
  | Performances
  | Stats
  | SignOut

t : Language -> Sentence -> List (Html msg)
t language line =
  case language of
    English ->
      case line of
        Welcome -> [text "Welcome!"]
        YouAreConnectedTo url -> [text "You are connected to ", code [] [text url], text "."]
        NoReasonToPanic -> [text "There is no reason to panic."]
        Composers -> [text "Composers"]
        Compositions -> [text "Compositions"]
        Performances -> [text "Performances"]
        Stats -> [text "Stats"]
        SignOut -> [text "Sign Out"]
    Russian ->
      case line of
        Welcome -> [text "Добро пожаловать!"]
        YouAreConnectedTo url -> [text "Вы подключены к ", code [] [text url] , text "."]
        NoReasonToPanic -> [text "Всё будет хорошо."]
        Composers -> [text "Композиторы"]
        Compositions -> [text "Композиции"]
        Performances -> [text "Исполнения"]
        Stats -> [text "Статистика"]
        SignOut -> [text "Выйти"]
