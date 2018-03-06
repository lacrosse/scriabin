port module Components.PageTitle exposing (..)


port title : String -> Cmd a


set : String -> Cmd a
set string =
    title string


reset : Cmd a
reset =
    set "Celeste"
