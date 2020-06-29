module Pages.NotFound exposing (view)

import Html exposing (Html, div, text)
import Html.Attributes exposing (class)


view : Html msg
view =
    div [ class "width-full height-full flex justify-center align-center font-size-xl color-light" ]
        [ text "Page not found" ]
