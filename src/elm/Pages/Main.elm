module Pages.Main exposing (Msg, update, view)

import Array exposing (Array)
import Entities.Drink exposing (Drink, viewDrinks)
import Entities.Meal exposing (Meal, viewMeals)
import Entities.Order exposing (OrderMealChange(..), updateOrder, viewOrder)
import Entities.Table exposing (Table, TableState(..), viewTables)
import Html exposing (Html, a, aside, button, div, text)
import Html.Attributes exposing (class, disabled, href)
import Html.Events exposing (onClick)



-- TYPE


type Msg
    = ChangeSelectedTable (Maybe Table)
    | IncrementCustomersForTable
    | DecrementCustomersForTable
    | ReserveTable Int Int
    | EmptyTable Int
    | UpdateOrder OrderMealChange Table


type alias Model a =
    { a
        | customersForTable : Int
        , selectedTable : Maybe Table
        , meals : Array Meal
        , drinks : Array Drink
        , tables : List Table
    }



-- UPDATE


handleTableReservation : Model a -> Int -> TableState -> Model a
handleTableReservation model tableId state =
    case
        List.filter (\table -> table.id == tableId) model.tables
            |> List.head
            |> Maybe.map (\table -> { table | state = state })
    of
        Just reservedTable ->
            { model
                | tables =
                    List.map
                        (\table ->
                            if table.id == tableId then
                                reservedTable

                            else
                                table
                        )
                        model.tables
                , selectedTable = Just reservedTable
                , customersForTable = 1
            }

        Nothing ->
            model


update : Msg -> Model a -> ( Model a, Cmd Msg )
update message model =
    case message of
        ChangeSelectedTable table ->
            ( { model | selectedTable = table, customersForTable = 1 }, Cmd.none )

        IncrementCustomersForTable ->
            ( { model | customersForTable = Basics.min 4 (model.customersForTable + 1) }, Cmd.none )

        DecrementCustomersForTable ->
            ( { model | customersForTable = Basics.max 1 (model.customersForTable - 1) }, Cmd.none )

        ReserveTable tableId customerCount ->
            ( handleTableReservation model tableId (HasCustomers customerCount), Cmd.none )

        EmptyTable tableId ->
            ( handleTableReservation model tableId Empty, Cmd.none )

        UpdateOrder orderMealChange table ->
            ( { model
                | selectedTable =
                    Just
                        { table
                            | order = updateOrder orderMealChange model.meals model.drinks table.order
                        }
              }
            , Cmd.none
            )



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
                                            (ReserveTable table.id customersForTable)
                                        ]
                                        [ text "Reserve table" ]
                                    , button [ class "button width-full ml-xs", onClick (ChangeSelectedTable Nothing) ]
                                        [ text "Cancel" ]
                                    ]
                                ]

                        HasCustomers _ ->
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
                                , div
                                    [ class "flex row" ]
                                    [ button [ class "button width-full mr-xs", onClick (EmptyTable table.id) ] [ text "Empty table" ]
                                    , button [ class "button width-full ml-xs", onClick (ChangeSelectedTable Nothing) ]
                                        [ text "Cancel" ]
                                    ]
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
