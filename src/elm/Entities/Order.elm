module Entities.Order exposing
    ( Order
    , OrderMeal
    , OrderMealChange(..)
    , OrderPaymentType(..)
    , empty
    , updateOrder
    , viewOrder
    )

import Array exposing (Array)
import Entities.Meal exposing (Meal)
import Html exposing (Html, div, hr, text)
import Html.Attributes exposing (class)
import Html.Keyed exposing (node)
import Html.Lazy exposing (lazy)
import Round



-- TYPES


type alias OrderMeal =
    { meal : Meal
    , count : Int
    }


type OrderPaymentType
    = Card
    | Cash
    | Cheque


type alias Order =
    { meals : List OrderMeal
    , paymentType : Maybe OrderPaymentType
    }


type OrderMealChange
    = OrderMealDecrement Int
    | OrderMealIncrement Int



-- CONSTRUCTORS


empty : Order
empty =
    Order [] Nothing



-- HELPERS


totalMealPayment : OrderMeal -> Float
totalMealPayment { count, meal } =
    toFloat count * meal.price


totalPaymentFold : OrderMeal -> Float -> Float
totalPaymentFold orderMeal accumulatedPrice =
    accumulatedPrice + totalMealPayment orderMeal


totalPayment : Order -> Float
totalPayment { meals } =
    List.foldl totalPaymentFold 0.0 meals



-- UPDATE


updateMealCount : Int -> Int -> OrderMeal -> OrderMeal
updateMealCount mealId increment orderMeal =
    if orderMeal.meal.id == mealId then
        { orderMeal | count = Basics.max 0 (orderMeal.count + increment) }

    else
        orderMeal


findMeal : Int -> Meal -> Maybe Meal -> Maybe Meal
findMeal mealId meal currentOutput =
    if meal.id == mealId then
        Just meal

    else
        currentOutput


filterEmptyOrderMeals : OrderMeal -> Bool
filterEmptyOrderMeals orderMeal =
    orderMeal.count > 0


updateOrder : OrderMealChange -> Array Meal -> Order -> Order
updateOrder msg allMeals order =
    case msg of
        OrderMealIncrement mealId ->
            { order
                | meals =
                    if
                        List.isEmpty
                            (List.filter (\orderMeal -> orderMeal.meal.id == mealId)
                                order.meals
                            )
                    then
                        case Array.foldl (findMeal mealId) Nothing allMeals of
                            Just meal ->
                                order.meals ++ [ { count = 1, meal = meal } ]

                            Nothing ->
                                order.meals

                    else
                        List.map (updateMealCount mealId 1) order.meals
            }

        OrderMealDecrement mealId ->
            { order
                | meals =
                    order.meals
                        |> List.map (updateMealCount mealId -1)
                        |> List.filter filterEmptyOrderMeals
            }



-- VIEW


viewOrderMeal : OrderMeal -> Html msg
viewOrderMeal { meal, count } =
    div [ class "flex mb-xs" ]
        [ div [ class "font-size-md" ] [ text meal.name ]
        , div [ class "flex-1 leaders mx-xs my-xs" ] []
        , div [ class "font-size-md" ]
            [ text
                (String.fromInt count
                    ++ " * "
                    ++ Round.floor 2 meal.price
                    ++ " = "
                    ++ Round.floor 2 (totalMealPayment { meal = meal, count = count })
                )
            ]
        ]


viewKeyedOrderMeals : OrderMeal -> ( String, Html msg )
viewKeyedOrderMeals orderMeal =
    ( String.fromInt orderMeal.meal.id, lazy viewOrderMeal orderMeal )


viewOrder : Order -> Html msg
viewOrder order =
    div []
        [ node "div"
            [ class "mb-md" ]
            (order.meals
                |> List.sortBy (.meal >> .name)
                |> List.map viewKeyedOrderMeals
            )
        , hr [ class "mb-md" ] []
        , div [ class "flex justify-between mb-md font-size-lg" ]
            [ div [] [ text "Total payment: " ]
            , div [] [ text (Round.floor 2 (totalPayment order)) ]
            ]
        ]
