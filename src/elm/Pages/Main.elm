module Pages.Main exposing (Msg, update, view)

import Array exposing (Array)
import Entities.Drink exposing (Drink, viewDrinks)
import Entities.Meal exposing (Meal, viewMeals)
import Entities.Order as Order exposing (OrderMealChange(..), OrderPaymentType(..), convertPaymentTypeToString, hasNothingOrdered, updateOrder, viewOrder)
import Entities.Table exposing (Table, TableState(..), viewTables)
import Html exposing (Html, a, aside, button, div, text)
import Html.Attributes exposing (class, disabled, href)
import Html.Events exposing (onClick)
import Http
import Json.Encode as Encode



-- TYPE


type Msg
    = ChangeSelectedTable (Maybe Table)
    | IncrementCustomersForTable
    | DecrementCustomersForTable
    | ReserveTable Table Int
    | EmptyTable Table
    | UpdateOrder OrderMealChange Table
    | StartPayment Table
    | CancelPayment Table
    | FinishPayment OrderPaymentType Table
    | PaymentAdded Table (Result Http.Error ())


type alias Model a =
    { a
        | customersForTable : Int
        , apiUrl : String
        , selectedTable : Maybe Table
        , meals : Array Meal
        , drinks : Array Drink
        , tables : List Table
    }



-- UPDATE


updateTableAtId : Table -> List Table -> List Table
updateTableAtId updatedTable tables =
    List.map
        (\table ->
            if table.id == updatedTable.id then
                updatedTable

            else
                table
        )
        tables


handleTableReservation : Model a -> Table -> TableState -> Model a
handleTableReservation model table state =
    let
        updatedTable =
            { table
                | state = state
                , order = Order.empty
            }
    in
    { model
        | tables = updateTableAtId updatedTable model.tables
        , selectedTable = Just updatedTable
        , customersForTable = 1
    }


handlePaymentRequest : OrderPaymentType -> Table -> Model a -> Cmd Msg
handlePaymentRequest paymentType table { apiUrl } =
    Http.request
        { url = apiUrl ++ "/payments"
        , headers =
            [ Http.header "Access-Control-Allow-Origin" "*"
            ]
        , expect = Http.expectWhatever (PaymentAdded table)
        , method = "POST"
        , timeout = Nothing
        , tracker = Nothing
        , body =
            Http.jsonBody
                (Encode.object
                    [ ( "tableId", Encode.int table.id )
                    , ( "customerCount", Encode.int table.customerCount )
                    , ( "paymentType"
                      , paymentType
                            |> convertPaymentTypeToString
                            |> Encode.string
                      )
                    , ( "orderedMeals"
                      , Encode.list
                            (\orderMeal ->
                                Encode.object
                                    [ ( "count", Encode.int orderMeal.count )
                                    , ( "meal"
                                      , Encode.object
                                            [ ( "id", Encode.int orderMeal.meal.id )
                                            , ( "name", Encode.string orderMeal.meal.name )
                                            , ( "price", Encode.float orderMeal.meal.price )
                                            ]
                                      )
                                    ]
                            )
                            table.order.meals
                      )
                    , ( "orderedDrinks"
                      , Encode.list
                            (\orderDrink ->
                                Encode.object
                                    [ ( "count", Encode.int orderDrink.count )
                                    , ( "drink"
                                      , Encode.object
                                            [ ( "id", Encode.int orderDrink.drink.id )
                                            , ( "name", Encode.string orderDrink.drink.name )
                                            , ( "volume", Encode.int orderDrink.drink.volume )
                                            , ( "price", Encode.float orderDrink.drink.price )
                                            ]
                                      )
                                    ]
                            )
                            table.order.drinks
                      )
                    ]
                )
        }


update : Msg -> Model a -> ( Model a, Cmd Msg )
update message model =
    case message of
        ChangeSelectedTable table ->
            ( { model | selectedTable = table, customersForTable = 1 }, Cmd.none )

        IncrementCustomersForTable ->
            ( { model | customersForTable = Basics.min 4 (model.customersForTable + 1) }, Cmd.none )

        DecrementCustomersForTable ->
            ( { model | customersForTable = Basics.max 1 (model.customersForTable - 1) }, Cmd.none )

        ReserveTable table customerCount ->
            let
                reservedTable =
                    { table
                        | customerCount = customerCount
                    }
            in
            ( handleTableReservation model reservedTable HasCustomers, Cmd.none )

        EmptyTable table ->
            let
                emptiedTable =
                    { table
                        | customerCount = 0
                    }
            in
            ( handleTableReservation model emptiedTable Empty, Cmd.none )

        UpdateOrder orderMealChange currentTable ->
            let
                updatedTable =
                    { currentTable
                        | order = updateOrder orderMealChange model.meals model.drinks currentTable.order
                    }
            in
            ( { model
                | selectedTable = Just updatedTable
                , tables = updateTableAtId updatedTable model.tables
              }
            , Cmd.none
            )

        StartPayment currentTable ->
            let
                updatedTable =
                    { currentTable
                        | state = HasStartedPayment
                    }
            in
            ( { model
                | selectedTable = Just updatedTable
                , tables = updateTableAtId updatedTable model.tables
              }
            , Cmd.none
            )

        CancelPayment currentTable ->
            let
                updatedTable =
                    { currentTable
                        | state = HasCustomers
                    }
            in
            ( { model
                | selectedTable = Just updatedTable
                , tables = updateTableAtId updatedTable model.tables
              }
            , Cmd.none
            )

        FinishPayment paymentType table ->
            ( model, handlePaymentRequest paymentType table model )

        PaymentAdded table response ->
            case response of
                Ok _ ->
                    let
                        updatedTable =
                            { table | state = HasFinishedPayment }
                    in
                    ( { model
                        | selectedTable = Just updatedTable
                        , tables = updateTableAtId updatedTable model.tables
                      }
                    , Cmd.none
                    )

                Err _ ->
                    ( model, Cmd.none )



-- VIEW


viewSidePanel : Model a -> Html Msg
viewSidePanel { customersForTable, selectedTable, meals, drinks } =
    aside [ class "side-panel flex column" ]
        [ div [ class "py-md px-md flex justify-between" ]
            [ div [ class "font-size-xl font-family-cursive" ]
                [ text "Bolec Gastro" ]
            , a
                [ class "button font-size-lg", href "/admin" ]
                [ text "Settings" ]
            ]
        , case selectedTable of
            Just table ->
                div [ class "table-settings-container font-size-xl flex column flex-1 overflow-y-hidden" ]
                    [ div [ class "mb-lg text-center" ] [ text table.name ]
                    , case table.state of
                        Empty ->
                            div []
                                [ div [ class "mb-sm font-size-md text-center" ] [ text "Customer count" ]
                                , div [ class "mb-sm grid cols-3 gap-sm text-center" ]
                                    [ button
                                        [ class "button"
                                        , onClick DecrementCustomersForTable
                                        , disabled (customersForTable == 1)
                                        ]
                                        [ text "-" ]
                                    , div [] [ text (String.fromInt customersForTable) ]
                                    , button
                                        [ class "button"
                                        , onClick IncrementCustomersForTable
                                        , disabled (customersForTable == 4)
                                        ]
                                        [ text "+" ]
                                    ]
                                , div
                                    [ class "flex row" ]
                                    [ button
                                        [ class "button width-full mr-xs"
                                        , onClick
                                            (ReserveTable table customersForTable)
                                        ]
                                        [ text "Reserve table" ]
                                    , button [ class "button width-full ml-xs", onClick (ChangeSelectedTable Nothing) ]
                                        [ text "Cancel" ]
                                    ]
                                ]

                        HasCustomers ->
                            div [ class "flex column flex-1 overflow-y-hidden" ]
                                [ div [ class "flex-1 overflow-y-auto" ]
                                    [ viewMeals
                                        (\mealId -> UpdateOrder (OrderMealIncrement mealId) table)
                                        (\mealId -> UpdateOrder (OrderMealDecrement mealId) table)
                                        meals
                                    , viewDrinks
                                        (\drinkId -> UpdateOrder (OrderDrinkIncrement drinkId) table)
                                        (\drinkId -> UpdateOrder (OrderDrinkDecrement drinkId) table)
                                        drinks
                                    ]
                                , viewOrder table.order
                                , button
                                    [ class "button mb-sm"
                                    , onClick (StartPayment table)
                                    , disabled
                                        (hasNothingOrdered table.order)
                                    ]
                                    [ text "Finish order" ]
                                , div
                                    [ class "flex row" ]
                                    [ button [ class "button width-full mr-xs", onClick (EmptyTable table) ] [ text "Empty table" ]
                                    , button [ class "button width-full ml-xs", onClick (ChangeSelectedTable Nothing) ]
                                        [ text "Cancel" ]
                                    ]
                                ]

                        HasStartedPayment ->
                            div [ class "flex column" ]
                                [ div [ class "color-light font-size-lg mb-md" ] [ text "Choose payment method" ]
                                , button
                                    [ class "button width-full mb-md"
                                    , onClick (FinishPayment Card table)
                                    ]
                                    [ text "Card" ]
                                , button
                                    [ class "button width-full mb-md"
                                    , onClick (FinishPayment Cash table)
                                    ]
                                    [ text "Cash" ]
                                , button
                                    [ class "button width-fullwidth mb-lg"
                                    , onClick (FinishPayment Cheque table)
                                    ]
                                    [ text "Cheque" ]
                                , button [ class "button width-full", onClick (CancelPayment table) ]
                                    [ text "Cancel" ]
                                ]

                        HasFinishedPayment ->
                            div [ class "flex" ]
                                [ button [ class "button width-full", onClick (EmptyTable table) ]
                                    [ text "Empty table" ]
                                ]
                    ]

            Nothing ->
                div [ class "font-size-lg flex-1 flex justify-center align-center" ] [ text "No table selected" ]
        ]


view : Model a -> Html Msg
view model =
    div [ class "flex width-full main-container" ]
        [ viewTables (\table -> ChangeSelectedTable (Just table)) model.tables
        , viewSidePanel model
        ]
