module Entities.Order exposing
    ( Order
    , OrderMeal
    , OrderMealChange(..)
    , OrderPaymentType(..)
    , empty
    , updateOrder
    , viewOrder
    )

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
        { orderMeal | count = orderMeal.count + increment }

    else
        orderMeal


updateOrder : OrderMealChange -> Order -> Order
updateOrder msg order =
    case msg of
        OrderMealIncrement mealId ->
            { order | meals = List.map (updateMealCount mealId 1) order.meals }

        OrderMealDecrement mealId ->
            { order | meals = List.map (updateMealCount mealId -1) order.meals }



-- VIEW


viewOrderMeal : OrderMeal -> Html msg
viewOrderMeal { meal, count } =
    div [ class "flex justify-between" ]
        [ div [ class "font-size-md" ] [ text meal.name ]
        , div [ class "font-size-sm" ]
            [ text
                (String.fromInt count
                    ++ " * "
                    ++ Round.floor 2 meal.price
                    ++ " = "
                    ++ Round.floor 2 (totalMealPayment { meal = meal, count = count })
                )
            ]
        ]


viewKeyedOrderMeals : Int -> OrderMeal -> ( String, Html msg )
viewKeyedOrderMeals index orderMeal =
    ( String.fromInt index, lazy viewOrderMeal orderMeal )


viewOrder : Order -> Html msg
viewOrder order =
    div []
        [ node "div" [ class "my-sm" ] (List.indexedMap viewKeyedOrderMeals order.meals)
        , hr [] []
        , div [ class "flex justify-between" ]
            [ div [] [ text "Total payment: " ]
            , div [] [ text (Round.floor 2 (totalPayment order) ++ "zł") ]
            ]
        ]
