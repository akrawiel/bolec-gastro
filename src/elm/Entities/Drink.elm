module Entities.Drink exposing (Drink, DrinkRequestMethods, DrinkRequestMsg, getDrinkRequester, updateDrinks, viewDrinks)

import Array exposing (Array)
import Html exposing (Attribute, Html, button, div, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Html.Keyed as Keyed
import Html.Lazy exposing (lazy3)
import Http
import Json.Decode exposing (Decoder, array, float, int, string, succeed)
import Json.Decode.Pipeline exposing (required)
import Round



-- TYPES


type alias Drink =
    { id : Int
    , name : String
    , price : Float
    , volume : Int
    }


type alias DrinkRequestMethods =
    { getAllDrinks : Cmd DrinkRequestMsg
    }


type DrinkRequestMsg
    = GotAllDrinksResponse (Result Http.Error (Array Drink))



-- JSON DECODERS


drinkDecoder : Decoder (Array Drink)
drinkDecoder =
    array
        (succeed Drink
            |> required "id" int
            |> required "name" string
            |> required "price" float
            |> required "volume" int
        )



-- REQUESTS GENERATOR


getDrinkRequester : String -> DrinkRequestMethods
getDrinkRequester apiUrl =
    { getAllDrinks =
        Http.request
            { url = apiUrl ++ "/drinks"
            , headers =
                [ Http.header "Access-Control-Allow-Origin" "*"
                ]
            , expect = Http.expectJson GotAllDrinksResponse drinkDecoder
            , method = "GET"
            , timeout = Nothing
            , tracker = Nothing
            , body = Http.emptyBody
            }
    }



-- UPDATE


updateDrinks : DrinkRequestMsg -> Array Drink -> Array Drink
updateDrinks msg default =
    case msg of
        GotAllDrinksResponse response ->
            Result.withDefault default response



-- VIEW


viewDrink : (Int -> msg) -> (Int -> msg) -> Drink -> Html msg
viewDrink onIncrementClick onDecrementClick drink =
    div [ class "meals__container gap-md mb-md" ]
        [ div [ class "meals__card p-sm border-radius-sm" ]
            [ div [ class "font-size-lg" ] [ text drink.name ]
            , div [ class "font-size-sm" ] [ text (Round.floor 2 drink.price ++ "zł") ]
            ]
        , button [ class "p-none", onClick (onDecrementClick drink.id) ] [ text "-" ]
        , button [ class "p-none", onClick (onIncrementClick drink.id) ] [ text "+" ]
        ]


viewKeyedDrinks : (Int -> msg) -> (Int -> msg) -> Drink -> ( String, Html msg )
viewKeyedDrinks onIncrementClick onDecrementClick drink =
    ( String.fromInt drink.id, lazy3 viewDrink onIncrementClick onDecrementClick drink )


viewDrinks : (Int -> msg) -> (Int -> msg) -> Array Drink -> Html msg
viewDrinks onIncrementClick onDecrementClick drinks =
    div [ class "flex column" ]
        [ div [ class "mb-sm font-size-lg text-center" ] [ text "Drinks" ]
        , Keyed.node "div"
            []
            (drinks
                |> Array.toList
                |> List.sortBy .name
                |> List.map
                    (viewKeyedDrinks onIncrementClick
                        onDecrementClick
                    )
            )
        ]
