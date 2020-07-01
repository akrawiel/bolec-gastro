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


type alias Countable a =
    { a | count : Int }


type alias OrderMeal =
    Countable { meal : Meal }


type alias OrderDrink =
    Countable { drink : Drink }


type alias Indexable a =
    { a | id : Int }


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
updateDrinkCount drinkId increment orderDrink =
    if orderDrink.drink.id == drinkId then
        { orderDrink | count = Basics.max 0 (orderDrink.count + increment) }

    else
        orderDrink


findEntity : Int -> Indexable a -> Maybe (Indexable a) -> Maybe (Indexable a)
findEntity entityId entity currentOutput =
    if entity.id == entityId then
        Just entity

    else
        currentOutput


filterEmptyOrderEntities : Countable a -> Bool
filterEmptyOrderEntities { count } =
    count > 0


newOrderMealConstructor : Meal -> OrderMeal
newOrderMealConstructor meal =
    { count = 1
    , meal = meal
    }


newOrderDrinkConstructor : Drink -> OrderDrink
newOrderDrinkConstructor drink =
    { count = 1
    , drink = drink
    }


decrementOrderEntities :
    (Int -> Int -> Countable a -> Countable a)
    -> Int
    -> List (Countable a)
    -> List (Countable a)
decrementOrderEntities updateMethod entityId countables =
    countables
        |> List.map (updateMethod entityId -1)
        |> List.filter filterEmptyOrderEntities


incrementOrderEntities :
    (Int -> Int -> Countable a -> Countable a)
    -> Int
    -> Array (Indexable b)
    -> (Countable a -> Int)
    -> (Indexable b -> Countable a)
    -> List (Countable a)
    -> List (Countable a)
incrementOrderEntities updateMethod entityId indexables idGetter entityConstructor countables =
    case
        List.filter (\entity -> idGetter entity == entityId) countables
    of
        [] ->
            Array.foldl (findEntity entityId) Nothing indexables
                |> Maybe.map (\entity -> countables ++ [ entityConstructor entity ])
                |> Maybe.withDefault countables

        _ ->
            List.map (updateMethod entityId 1) countables


updateOrder : OrderMealChange -> Array Meal -> Array Drink -> Order -> Order
updateOrder msg allMeals allDrinks order =
    case msg of
        OrderMealIncrement entityId ->
            { order
                | meals =
                    incrementOrderEntities updateMealCount
                        entityId
                        allMeals
                        (.meal >> .id)
                        newOrderMealConstructor
                        order.meals
            }

        OrderMealDecrement entityId ->
            { order
                | meals = decrementOrderEntities updateMealCount entityId order.meals
            }

        OrderDrinkIncrement entityId ->
            { order
                | drinks =
                    incrementOrderEntities updateDrinkCount
                        entityId
                        allDrinks
                        (.drink >> .id)
                        newOrderDrinkConstructor
                        order.drinks
            }

        OrderDrinkDecrement entityId ->
            { order
                | drinks = decrementOrderEntities updateDrinkCount entityId order.drinks
            }



-- VIEW


viewCountable : { count : Int, name : String, price : Float, totalPrice : Float } -> Html msg
viewCountable { count, name, price, totalPrice } =
    div [ class "flex mb-xs" ]
        [ div [ class "font-size-md" ] [ text name ]
        , div [ class "flex-1 leaders mx-xs my-xs" ] []
        , div [ class "font-size-md" ]
            [ text
                (String.fromInt count
                    ++ " * "
                    ++ Round.floor 2 price
                    ++ " = "
                    ++ Round.floor 2 totalPrice
                )
            ]
        ]


viewOrderMeal : OrderMeal -> Html msg
viewOrderMeal { meal, count } =
    viewCountable
        { count = count
        , name = meal.name
        , price = meal.price
        , totalPrice = totalMealPayment { meal = meal, count = count }
        }


viewKeyedOrderMeals : OrderMeal -> ( String, Html msg )
viewKeyedOrderMeals orderMeal =
    ( String.fromInt orderMeal.meal.id, lazy viewOrderMeal orderMeal )


viewOrderDrink : OrderDrink -> Html msg
viewOrderDrink { drink, count } =
    viewCountable
        { count = count
        , name = drink.name
        , price = drink.price
        , totalPrice = totalDrinkPayment { drink = drink, count = count }
        }


viewKeyedOrderDrinks : OrderDrink -> ( String, Html msg )
viewKeyedOrderDrinks orderDrink =
    ( String.fromInt orderDrink.drink.id, lazy viewOrderDrink orderDrink )


viewOrder : Order -> Html msg
viewOrder order =
    div []
        [ node "div"
            [ class "mt-sm" ]
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
        , hr [ class "my-sm" ] []
        , div [ class "flex justify-between mb-md font-size-lg" ]
            [ div [] [ text "Total payment: " ]
            , div [] [ text (Round.floor 2 (totalPayment order)) ]
            ]
        ]
