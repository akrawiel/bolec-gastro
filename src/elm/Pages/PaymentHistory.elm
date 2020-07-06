module Pages.PaymentHistory exposing (view)

import Array exposing (Array)
import Entities.Order exposing (OrderDrink, OrderMeal, viewOrderDrink, viewOrderMeal)
import Entities.Payment exposing (Payment)
import Html exposing (Html, a, b, div, em, hr, span, text)
import Html.Attributes exposing (class, href)
import Html.Keyed as Keyed
import Html.Lazy exposing (lazy, lazy2)
import Time exposing (utc)


type alias Model a =
    { a
        | payments : Array Payment
        , timeZone : Maybe Time.Zone
    }



-- VIEW


getNumericMonth : Time.Month -> String
getNumericMonth month =
    case month of
        Time.Jan ->
            "01"

        Time.Feb ->
            "02"

        Time.Mar ->
            "03"

        Time.Apr ->
            "04"

        Time.May ->
            "05"

        Time.Jun ->
            "06"

        Time.Jul ->
            "07"

        Time.Aug ->
            "08"

        Time.Sep ->
            "09"

        Time.Oct ->
            "10"

        Time.Nov ->
            "11"

        Time.Dec ->
            "12"


getStringWeekday : Time.Weekday -> String
getStringWeekday weekday =
    case weekday of
        Time.Mon ->
            "Mon"

        Time.Tue ->
            "Tue"

        Time.Wed ->
            "Wed"

        Time.Thu ->
            "Thu"

        Time.Fri ->
            "Fri"

        Time.Sat ->
            "Sat"

        Time.Sun ->
            "Sun"


zeroPad : String -> String
zeroPad fragment =
    String.padLeft 2 '0' fragment


getAmOrPm : Int -> String
getAmOrPm hour =
    if hour >= 12 then
        "PM"

    else
        "AM"


getAmPmAdjustedHour : Int -> Int
getAmPmAdjustedHour hour =
    let
        zeroBasedHour =
            hour // 12
    in
    if zeroBasedHour == 0 then
        12

    else
        zeroBasedHour


formatTime : Maybe Time.Zone -> Time.Posix -> String
formatTime maybeTimeZone timestamp =
    let
        timeZone =
            Maybe.withDefault utc maybeTimeZone
    in
    (Time.toWeekday timeZone timestamp |> getStringWeekday)
        ++ ", "
        ++ getNumericMonth (Time.toMonth timeZone timestamp)
        ++ "/"
        ++ (Time.toDay timeZone timestamp
                |> String.fromInt
                |> zeroPad
           )
        ++ "/"
        ++ String.fromInt (Time.toYear timeZone timestamp)
        ++ " "
        ++ (Time.toHour timeZone timestamp
                |> getAmPmAdjustedHour
                |> String.fromInt
                |> zeroPad
           )
        ++ ":"
        ++ (Time.toMinute timeZone timestamp
                |> String.fromInt
                |> zeroPad
           )
        ++ ":"
        ++ (Time.toSecond timeZone timestamp
                |> String.fromInt
                |> zeroPad
           )
        ++ " "
        ++ (Time.toHour timeZone timestamp
                |> getAmOrPm
           )


viewKeyedOrderMeal : OrderMeal -> ( String, Html msg )
viewKeyedOrderMeal orderMeal =
    ( String.fromInt orderMeal.meal.id, lazy viewOrderMeal orderMeal )


viewKeyedOrderDrink : OrderDrink -> ( String, Html msg )
viewKeyedOrderDrink orderDrink =
    ( String.fromInt orderDrink.drink.id, lazy viewOrderDrink orderDrink )


viewPayment : Maybe Time.Zone -> Payment -> Html msg
viewPayment timeZone payment =
    div [ class "flex column color-light" ]
        [ hr [ class "my-md" ] []
        , b [ class "font-size-lg" ]
            [ div []
                [ text ("ID " ++ String.fromInt payment.id ++ ", Table " ++ String.fromInt payment.tableId)
                ]
            , div [] [ text (formatTime timeZone payment.createdAtTimestamp) ]
            ]
        , div [ class "my-xs" ] [ text ("Customers: " ++ String.fromInt payment.customerCount) ]
        , em [ class "mt-sm" ] [ text "Ordered meals" ]
        , if List.isEmpty payment.orderedMeals then
            div [] [ text "None" ]

          else
            Keyed.node "div"
                [ class "my-xs" ]
                (payment.orderedMeals
                    |> List.sortBy (.meal >> .name)
                    |> List.map viewKeyedOrderMeal
                )
        , em [ class "mt-sm" ] [ text "Ordered drinks" ]
        , if List.isEmpty payment.orderedDrinks then
            div [] [ text "None" ]

          else
            Keyed.node "div"
                [ class "my-xs" ]
                (payment.orderedDrinks
                    |> List.sortBy (.drink >> .name)
                    |> List.map viewKeyedOrderDrink
                )
        ]


viewKeyedPayments : Maybe Time.Zone -> Payment -> ( String, Html msg )
viewKeyedPayments timeZone payment =
    ( String.fromInt payment.id, lazy2 viewPayment timeZone payment )


viewPayments : Maybe Time.Zone -> Array Payment -> Html msg
viewPayments timeZone payments =
    div [ class "flex p-md column" ]
        [ div [ class "color-light font-size-lg mb-md font-style-italic" ] [ text "Payment history" ]
        , if Array.isEmpty payments then
            div [ class "color-light" ] [ text "No payments in the database" ]

          else
            Keyed.node "div"
                []
                (payments
                    |> Array.toList
                    |> List.sortBy (.createdAtTimestamp >> Time.posixToMillis)
                    |> List.reverse
                    |> List.map (viewKeyedPayments timeZone)
                )
        ]


view : Model a -> Html msg
view { payments, timeZone } =
    div [ class "bg-secondary min-height-full sizing-border" ]
        [ div [ class "mb-md flex justify-between p-md" ]
            [ div [ class "font-size-xl font-family-cursive color-light" ]
                [ text "Bolec Gastro" ]
            , div [ class "flex" ]
                [ a
                    [ class "button font-size-lg mr-sm", href "/admin" ]
                    [ text "Settings" ]
                , a
                    [ class "button font-size-lg", href "/" ]
                    [ text "Home" ]
                ]
            ]
        , viewPayments timeZone payments
        ]
