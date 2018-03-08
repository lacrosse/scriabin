module Connection.Session exposing (..)

import Json.Decode as JD
import Json.Encode as JE
import Connection.Server.Types exposing (..)


decoder : JD.Decoder User
decoder =
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


encoder : Wannabe -> JE.Value
encoder { username, password } =
    JE.object
        [ ( "session"
          , JE.object
                [ ( "username", JE.string username )
                , ( "password", JE.string password )
                ]
          )
        ]
