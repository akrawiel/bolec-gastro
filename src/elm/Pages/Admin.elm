module Pages.Admin exposing (Msg, update, view)

import Array exposing (Array)
import Entities.Drink exposing (Drink)
import Entities.Meal exposing (Meal)
import Html exposing (Html, div, text)


type alias Model a =
    { a
        | meals : Array Meal
        , drinks : Array Drink
    }


type Msg
    = Change


update : Msg -> Model a -> ( Model a, Cmd Msg )
update msg model =
    ( model, Cmd.none )


view : Model a -> Html Msg
view model =
    div [] [ text "Admin" ]
