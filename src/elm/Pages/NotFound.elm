module Pages.NotFound exposing (view)

import Html exposing (Html, a, div, text)
import Html.Attributes exposing (class, href)


view : Html msg
view =
    div [ class "flex justify-center align-center column height-full color-light" ]
        [ div [ class "font-size-xl font-family-cursive mb-xl" ] [ text "Bolec Gastro" ]
        , div [ class "font-size-lg mb-md" ] [ text "Page not found" ]
        , a [ class "button font-size-lg", href "/" ] [ text "Go to main page" ]
        ]
