module Pages.Admin exposing (Msg, update, view)

import Array exposing (Array)
import Entities.Drink exposing (Drink)
import Entities.Meal exposing (Meal)
import Html exposing (Html, a, div, text)
import Html.Attributes exposing (class, href)
import Pages.Admin.Drinks exposing (viewAddOrEditDrink, viewDrinks)
import Pages.Admin.Meals exposing (viewAddOrEditMeal, viewMeals)



-- TYPES


type alias Model a =
    { a
        | apiUrl : String
        , meals : Array Meal
        , drinks : Array Drink
        , currentlyEditedMeal : Maybe Meal
        , currentlyEditedDrink : Maybe Drink
        , currentlyEditedName : String
        , currentlyEditedPrice : String
        , currentlyEditedVolume : String
        , addingMeal : Bool
        , addingDrink : Bool
    }


type Msg
    = MealsUpdateMsg Pages.Admin.Meals.Msg
    | DrinksUpdateMsg Pages.Admin.Drinks.Msg



-- UPDATE


update : Msg -> Model a -> ( Model a, Cmd Msg )
update msg model =
    case msg of
        MealsUpdateMsg message ->
            Pages.Admin.Meals.update message model |> Tuple.mapSecond (Cmd.map MealsUpdateMsg)

        DrinksUpdateMsg message ->
            Pages.Admin.Drinks.update message model |> Tuple.mapSecond (Cmd.map DrinksUpdateMsg)



-- VIEW


viewEditedEntry : Model a -> Html Msg
viewEditedEntry model =
    let
        editingMeal =
            model.currentlyEditedMeal
                |> Maybe.map (always True)
                |> Maybe.withDefault False

        editingDrink =
            model.currentlyEditedDrink
                |> Maybe.map (always True)
                |> Maybe.withDefault False

        addingMeal =
            model.addingMeal

        addingDrink =
            model.addingDrink
    in
    case ( addingMeal || editingMeal, addingDrink || editingDrink ) of
        ( True, False ) ->
            Html.map MealsUpdateMsg (viewAddOrEditMeal model)

        ( False, True ) ->
            Html.map DrinksUpdateMsg (viewAddOrEditDrink model)

        _ ->
            div [ class "flex-1 color-light font-size-xl text-center p-md" ] [ text "No entry selected" ]


view : Model a -> Html Msg
view model =
    div [ class "bg-secondary min-height-full sizing-border" ]
        [ div [ class "mb-md flex justify-between p-md" ]
            [ div [ class "font-size-xl font-family-cursive color-light" ]
                [ text "Bolec Gastro" ]
            , div [ class "flex" ]
                [ a
                    [ class "button font-size-lg mr-sm", href "/payment-history" ]
                    [ text "Payment history" ]
                , a
                    [ class "button font-size-lg", href "/" ]
                    [ text "Home" ]
                ]
            ]
        , div [ class "flex admin-entries-container" ]
            [ div [ class "flex-1 p-md" ]
                [ Html.map MealsUpdateMsg (viewMeals model)
                , Html.map DrinksUpdateMsg (viewDrinks model)
                ]
            , viewEditedEntry model
            ]
        ]
