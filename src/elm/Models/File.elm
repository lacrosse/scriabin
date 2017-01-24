module Models.File exposing (..)

-- MODEL

type alias File =
  { id : Int
  , sha256 : String
  , mime : String
  , name : String
  }

dummy : File
dummy =
  File 1 "4e1243bd22c66e76c2ba9eddc1f91394e57f9f83" "audio/mpeg" "01.mp3"
