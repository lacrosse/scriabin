module I18n exposing (..)

import Html exposing (Html, text, code)
import Connection.Server.Types exposing (Endpoint)
import Celeste.Url as Url


type Language
    = English
    | Russian


type Sentence
    = Welcome
    | YouAreConnectedTo Endpoint
    | NoReasonToPanic
    | Composers
    | Compositions
    | Performances
    | Stats
    | SignIn
    | SignInVerb
    | SignOut
    | NotFoundHeader
    | TryAgain
    | Profile


t : Language -> Sentence -> List (Html msg)
t language line =
    case language of
        English ->
            case line of
                Welcome ->
                    [ text "Welcome!" ]

                YouAreConnectedTo endpoint ->
                    [ text "You are connected to ", code [] [ text (Url.api endpoint) ], text "." ]

                NoReasonToPanic ->
                    [ text "There is no reason to panic." ]

                Composers ->
                    [ text "Composers" ]

                Compositions ->
                    [ text "Compositions" ]

                Performances ->
                    [ text "Performances" ]

                Stats ->
                    [ text "Stats" ]

                SignIn ->
                    [ text "Sign In" ]

                SignInVerb ->
                    t language SignIn

                SignOut ->
                    [ text "Sign Out" ]

                NotFoundHeader ->
                    [ text "Not Found" ]

                TryAgain ->
                    [ text "Try again." ]

                Profile ->
                    [ text "Profile" ]

        Russian ->
            case line of
                Welcome ->
                    [ text "Добро пожаловать!" ]

                YouAreConnectedTo endpoint ->
                    [ text "Вы подключены к ", code [] [ text (Url.api endpoint) ], text "." ]

                NoReasonToPanic ->
                    [ text "Всё будет хорошо." ]

                Composers ->
                    [ text "Композиторы" ]

                Compositions ->
                    [ text "Композиции" ]

                Performances ->
                    [ text "Исполнения" ]

                Stats ->
                    [ text "Статистика" ]

                SignIn ->
                    [ text "Вход" ]

                SignInVerb ->
                    [ text "Войти" ]

                SignOut ->
                    [ text "Выход" ]

                NotFoundHeader ->
                    [ text "Не найдено" ]

                TryAgain ->
                    [ text "Попробуйте сызнова." ]

                Profile ->
                    [ text "Профиль" ]


languageFlag : Language -> String
languageFlag lang =
    case lang of
        Russian ->
            "🇷🇺"

        English ->
            "🇺🇸"


languageName : Language -> Language -> String
languageName lang describedLang =
    case lang of
        Russian ->
            case describedLang of
                Russian ->
                    "русский"

                English ->
                    "английский"

        English ->
            case describedLang of
                Russian ->
                    "Russian"

                English ->
                    "English"


languageDescription : Language -> Language -> String
languageDescription lang describedLang =
    languageFlag describedLang ++ " " ++ languageName lang describedLang
