module Main exposing (main)

import Array exposing (Array)
import Browser exposing (Document)
import Browser.Navigation as Nav
import Entities.Drink
    exposing
        ( Drink
        , DrinkRequestMethods
        , DrinkRequestMsg
        , getDrinkRequester
        , updateDrinks
        , viewDrinks
        )
import Entities.Meal
    exposing
        ( Meal
        , MealRequestMethods
        , MealRequestMsg
        , getMealRequester
        , updateMeals
        , viewMeals
        )
import Entities.Order exposing (OrderMealChange(..), updateOrder, viewOrder)
import Entities.Table exposing (Table, TableState(..), mapTableForIndex, viewTables)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Url


main : Program { apiUrl : String } Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }



-- MODEL


type alias Model =
    { key : Nav.Key
    , url : Url.Url
    , tables : List Table
    , selectedTable : Maybe Table
    , customersForTable : Int
    , meals : Array Meal
    , drinks : Array Drink
    , mealRequester : MealRequestMethods
    , drinkRequester : DrinkRequestMethods
    }


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | ChangeSelectedTable (Maybe Table)
    | IncrementCustomersForTable
    | DecrementCustomersForTable
    | ReserveTable Int Int
    | EmptyTable Int
    | MealMsg MealRequestMsg
    | DrinkMsg DrinkRequestMsg
    | UpdateOrder OrderMealChange Table



-- INIT


init : { apiUrl : String } -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init { apiUrl } url key =
    let
        mealRequester =
            getMealRequester apiUrl

        drinkRequester =
            getDrinkRequester apiUrl
    in
    ( { key = key
      , url = url
      , tables = List.map mapTableForIndex (List.range 1 16)
      , selectedTable = Nothing
      , customersForTable = 1
      , meals = Array.empty
      , drinks = Array.empty
      , mealRequester = mealRequester
      , drinkRequester = drinkRequester
      }
    , Cmd.batch
        [ Cmd.map MealMsg mealRequester.getAllMeals
        , Cmd.map DrinkMsg drinkRequester.getAllDrinks
        ]
    )



-- UPDATE


handleTableReservation : Model -> Int -> TableState -> Model
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked request ->
            case request of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            ( { model | url = url }, Cmd.none )

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

        MealMsg requestMessage ->
            let
                meals =
                    updateMeals requestMessage model.meals
            in
            ( { model | meals = meals }, Cmd.none )

        DrinkMsg requestMessage ->
            let
                drinks =
                    updateDrinks requestMessage model.drinks
            in
            ( { model | drinks = drinks }, Cmd.none )

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



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


viewSidePanel : Model -> Html Msg
viewSidePanel { customersForTable, selectedTable, meals, drinks } =
    aside [ class "side-panel" ]
        [ case selectedTable of
            Just table ->
                div [ class "table-settings-container font-size-xl flex column" ]
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
                                , div [ class "mb-sm" ]
                                    [ button
                                        [ class "button width-full"
                                        , onClick
                                            (ReserveTable table.id
                                                customersForTable
                                            )
                                        ]
                                        [ text "Reserve table" ]
                                    ]
                                ]

                        HasCustomers _ ->
                            div [ class "flex column flex-1" ]
                                [ div [ class "flex-1" ]
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
                                , button [ class "button width-full mb-sm", onClick (EmptyTable table.id) ] [ text "Empty table" ]
                                ]
                    , div []
                        [ button [ class "button width-full", onClick (ChangeSelectedTable Nothing) ]
                            [ text "Cancel" ]
                        ]
                    ]

            Nothing ->
                div [ class "font-size-lg" ] [ text "No table selected" ]
        ]


view : Model -> Document Msg
view model =
    { title = "Bolec Gastro"
    , body =
        [ div [ class "app-container" ]
            [ viewTables (\table -> ChangeSelectedTable (Just table)) model.tables
            , viewSidePanel model
            ]
        ]
    }
