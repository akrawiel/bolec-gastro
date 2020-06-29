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
import Entities.Drink exposing (Drink)
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


type alias OrderDrink =
    { drink : Drink
    , count : Int
    }


type OrderPaymentType
    = Card
    | Cash
    | Cheque


type alias Order =
    { meals : List OrderMeal
    , drinks : List OrderDrink
    , paymentType : Maybe OrderPaymentType
    }


type OrderMealChange
    = OrderMealDecrement Int
    | OrderMealIncrement Int
    | OrderDrinkDecrement Int
    | OrderDrinkIncrement Int



-- CONSTRUCTORS


empty : Order
empty =
    Order [] [] Nothing



-- HELPERS


totalMealPayment : OrderMeal -> Float
totalMealPayment { count, meal } =
    toFloat count * meal.price


totalMealPaymentFold : OrderMeal -> Float -> Float
totalMealPaymentFold orderMeal accumulatedPrice =
    accumulatedPrice + totalMealPayment orderMeal


totalDrinkPayment : OrderDrink -> Float
totalDrinkPayment { count, drink } =
    toFloat count * drink.price


totalDrinkPaymentFold : OrderDrink -> Float -> Float
totalDrinkPaymentFold orderDrink accumulatedPrice =
    accumulatedPrice + totalDrinkPayment orderDrink


totalPayment : Order -> Float
totalPayment { meals, drinks } =
    List.foldl totalMealPaymentFold 0.0 meals
        + List.foldl totalDrinkPaymentFold 0.0 drinks



-- UPDATE


updateMealCount : Int -> Int -> OrderMeal -> OrderMeal
updateMealCount mealId increment orderMeal =
    if orderMeal.meal.id == mealId then
        { orderMeal | count = Basics.max 0 (orderMeal.count + increment) }

    else
        orderMeal


updateDrinkCount : Int -> Int -> OrderDrink -> OrderDrink
updateDrinkCount mealId increment orderDrink =
    if orderDrink.drink.id == mealId then
        { orderDrink | count = Basics.max 0 (orderDrink.count + increment) }

    else
        orderDrink


findMeal : Int -> Meal -> Maybe Meal -> Maybe Meal
findMeal mealId meal currentOutput =
    if meal.id == mealId then
        Just meal

    else
        currentOutput


findDrink : Int -> Drink -> Maybe Drink -> Maybe Drink
findDrink drinkId drink currentOutput =
    if drink.id == drinkId then
        Just drink

    else
        currentOutput


filterEmptyOrderMeals : OrderMeal -> Bool
filterEmptyOrderMeals orderMeal =
    orderMeal.count > 0


filterEmptyOrderDrinks : OrderDrink -> Bool
filterEmptyOrderDrinks orderMeal =
    orderMeal.count > 0


updateOrder : OrderMealChange -> Array Meal -> Array Drink -> Order -> Order
updateOrder msg allMeals allDrinks order =
    case msg of
        OrderMealIncrement entityId ->
            { order
                | meals =
                    if
                        List.isEmpty
                            (List.filter (\orderMeal -> orderMeal.meal.id == entityId)
                                order.meals
                            )
                    then
                        case Array.foldl (findMeal entityId) Nothing allMeals of
                            Just meal ->
                                order.meals ++ [ { count = 1, meal = meal } ]

                            Nothing ->
                                order.meals

                    else
                        List.map (updateMealCount entityId 1) order.meals
            }

        OrderMealDecrement entityId ->
            { order
                | meals =
                    order.meals
                        |> List.map (updateMealCount entityId -1)
                        |> List.filter filterEmptyOrderMeals
            }

        OrderDrinkIncrement entityId ->
            { order
                | drinks =
                    if
                        List.isEmpty
                            (List.filter (\orderDrink -> orderDrink.drink.id == entityId)
                                order.drinks
                            )
                    then
                        case Array.foldl (findDrink entityId) Nothing allDrinks of
                            Just drink ->
                                order.drinks ++ [ { count = 1, drink = drink } ]

                            Nothing ->
                                order.drinks

                    else
                        List.map (updateDrinkCount entityId 1) order.drinks
            }

        OrderDrinkDecrement entityId ->
            { order
                | drinks =
                    order.drinks
                        |> List.map (updateDrinkCount entityId -1)
                        |> List.filter filterEmptyOrderDrinks
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


viewOrderDrink : OrderDrink -> Html msg
viewOrderDrink { drink, count } =
    div [ class "flex mb-xs" ]
        [ div [ class "font-size-md" ] [ text drink.name ]
        , div [ class "flex-1 leaders mx-xs my-xs" ] []
        , div [ class "font-size-md" ]
            [ text
                (String.fromInt count
                    ++ " * "
                    ++ Round.floor 2 drink.price
                    ++ " = "
                    ++ Round.floor 2 (totalDrinkPayment { drink = drink, count = count })
                )
            ]
        ]


viewKeyedOrderDrinks : OrderDrink -> ( String, Html msg )
viewKeyedOrderDrinks orderDrink =
    ( String.fromInt orderDrink.drink.id, lazy viewOrderDrink orderDrink )


viewOrder : Order -> Html msg
viewOrder order =
    div []
        [ node "div"
            []
            (order.meals
                |> List.sortBy (.meal >> .name)
                |> List.map viewKeyedOrderMeals
            )
        , node "div"
            []
            (order.drinks
                |> List.sortBy (.drink >> .name)
                |> List.map viewKeyedOrderDrinks
            )
        , hr [ class "my-md" ] []
        , div [ class "flex justify-between mb-md font-size-lg" ]
            [ div [] [ text "Total payment: " ]
            , div [] [ text (Round.floor 2 (totalPayment order)) ]
            ]
        ]
