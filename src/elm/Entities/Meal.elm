module Entities.Meal exposing
    ( Meal
    , MealRequestMethods
    , MealRequestMsg
    , getMealRequester
    , mealArrayDecoder
    , mealDecoder
    , updateMeals
    , viewMeals
    )

import Array exposing (Array)
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Html.Keyed as Keyed
import Html.Lazy exposing (lazy3)
import Http
import Json.Decode exposing (Decoder, array, float, int, string, succeed)
import Json.Decode.Pipeline exposing (required)
import Round



-- TYPES


type alias Meal =
    { id : Int
    , name : String
    , price : Float
    }


type alias MealRequestMethods =
    { getAllMeals : Cmd MealRequestMsg
    }


type MealRequestMsg
    = GotAllMealsResponse (Result Http.Error (Array Meal))



-- JSON DECODERS


mealDecoder : Decoder Meal
mealDecoder =
    succeed Meal
        |> required "id" int
        |> required "name" string
        |> required "price" float


mealArrayDecoder : Decoder (Array Meal)
mealArrayDecoder =
    array mealDecoder



-- REQUESTS GENERATOR


getMealRequester : String -> MealRequestMethods
getMealRequester apiUrl =
    { getAllMeals =
        Http.request
            { url = apiUrl ++ "/meals"
            , headers =
                [ Http.header "Access-Control-Allow-Origin" "*"
                ]
            , expect = Http.expectJson GotAllMealsResponse mealArrayDecoder
            , method = "GET"
            , timeout = Nothing
            , tracker = Nothing
            , body = Http.emptyBody
            }
    }



-- UPDATE


updateMeals : MealRequestMsg -> Array Meal -> Array Meal
updateMeals msg default =
    case msg of
        GotAllMealsResponse response ->
            Result.withDefault default response



-- VIEW


viewMeal : (Int -> msg) -> (Int -> msg) -> Meal -> Html msg
viewMeal onIncrementClick onDecrementClick meal =
    div [ class "meals__container gap-md mb-md" ]
        [ div [ class "meals__card p-sm border-radius-sm" ]
            [ div [ class "font-size-lg" ] [ text meal.name ]
            , div [ class "font-size-sm" ] [ text (Round.floor 2 meal.price) ]
            ]
        , button [ class "button p-none", onClick (onDecrementClick meal.id) ] [ text "-" ]
        , button [ class "button p-none", onClick (onIncrementClick meal.id) ] [ text "+" ]
        ]


viewKeyedMeals : (Int -> msg) -> (Int -> msg) -> Meal -> ( String, Html msg )
viewKeyedMeals onIncrementClick onDecrementClick meal =
    ( String.fromInt meal.id, lazy3 viewMeal onIncrementClick onDecrementClick meal )


viewMeals : (Int -> msg) -> (Int -> msg) -> Array Meal -> Html msg
viewMeals onIncrementClick onDecrementClick meals =
    div [ class "flex column" ]
        [ div [ class "mb-sm font-size-lg text-center" ] [ text "Meals" ]
        , Keyed.node "div"
            []
            (meals
                |> Array.toList
                |> List.sortBy .name
                |> List.map (viewKeyedMeals onIncrementClick onDecrementClick)
            )
        ]
