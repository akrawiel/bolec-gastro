module Entities.Meal exposing (Meal, MealRequestMethods, MealRequestMsg, getMealRequester, updateMeals, viewMeals)

import Array exposing (Array)
import Html exposing (Html, div, text)
import Html.Keyed as Keyed
import Html.Lazy exposing (lazy)
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


mealDecoder : Decoder (Array Meal)
mealDecoder =
    array
        (succeed Meal
            |> required "id" int
            |> required "name" string
            |> required "price" float
        )



-- REQUESTS GENERATOR


getMealRequester : String -> MealRequestMethods
getMealRequester apiUrl =
    { getAllMeals =
        Http.request
            { url = apiUrl ++ "/meals"
            , headers =
                [ Http.header "Access-Control-Allow-Origin" "*"
                ]
            , expect = Http.expectJson GotAllMealsResponse mealDecoder
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


viewMeal : Meal -> Html msg
viewMeal meal =
    div []
        [ div [] [ text meal.name ]
        , div [] [ text (Round.floor 2 meal.price ++ "zł") ]
        ]


viewKeyedMeals : Meal -> ( String, Html msg )
viewKeyedMeals meal =
    ( String.fromInt meal.id, lazy viewMeal meal )


viewMeals : Array Meal -> Html msg
viewMeals meals =
    Keyed.node "div" [] (Array.toList <| Array.map viewKeyedMeals meals)
