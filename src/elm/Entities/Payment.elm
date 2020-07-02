module Entities.Payment exposing
    ( Msg
    , Payment
    , addPayment
    , getAllPayments
    , paymentArrayDecoder
    , paymentDecoder
    , paymentTableEncoder
    , update
    )

import Array exposing (Array)
import Entities.Drink exposing (Drink)
import Entities.Meal exposing (Meal)
import Entities.Order exposing (OrderDrink, OrderMeal, OrderPaymentType, convertPaymentTypeToString, paymentTypeDecoder)
import Entities.Table exposing (Table, TableState(..))
import Http
import Iso8601
import Json.Decode exposing (Decoder, andThen, array, float, int, list, string, succeed)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode
import Time



-- TYPES


type alias Payment =
    { id : Int
    , tableId : Int
    , customerCount : Int
    , createdAtTimestamp : Time.Posix
    , paymentType : OrderPaymentType
    , orderedMeals : List OrderMeal
    , orderedDrinks : List OrderDrink
    }


type Msg
    = AllPaymentsFetched (Result Http.Error (Array Payment))
    | PaymentAdded Table (Result Http.Error Payment)


type alias Model a =
    { a
        | payments : Array Payment
        , selectedTable : Maybe Table
        , tables : List Table
    }



-- JSON


paymentTableEncoder : OrderPaymentType -> Table -> Time.Posix -> Encode.Value
paymentTableEncoder paymentType table time =
    Encode.object
        [ ( "tableId", Encode.int table.id )
        , ( "customerCount", Encode.int table.customerCount )
        , ( "createdAtTimestamp", Iso8601.encode time )
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


paymentDecoder : Decoder Payment
paymentDecoder =
    succeed Payment
        |> required "id" int
        |> required "tableId" int
        |> required "customerCount" int
        |> required "createdAtTimestamp" Iso8601.decoder
        |> required "paymentType" (string |> andThen paymentTypeDecoder)
        |> required "orderedMeals"
            (list
                (succeed OrderMeal
                    |> required "count" int
                    |> required "meal"
                        (succeed Meal
                            |> required "id" int
                            |> required "name" string
                            |> required "price" float
                        )
                )
            )
        |> required "orderedDrinks"
            (list
                (succeed OrderDrink
                    |> required "count" int
                    |> required "drink"
                        (succeed Drink
                            |> required "id" int
                            |> required "name" string
                            |> required "price" float
                            |> required "volume" int
                        )
                )
            )


paymentArrayDecoder : Decoder (Array Payment)
paymentArrayDecoder =
    array paymentDecoder



-- REQUESTS


getAllPayments : String -> Cmd Msg
getAllPayments apiUrl =
    Http.request
        { url = apiUrl ++ "/payments"
        , headers =
            [ Http.header "Access-Control-Allow-Origin" "*"
            ]
        , expect = Http.expectJson AllPaymentsFetched paymentArrayDecoder
        , method = "GET"
        , timeout = Nothing
        , tracker = Nothing
        , body = Http.emptyBody
        }


addPayment : String -> OrderPaymentType -> Table -> Time.Posix -> Cmd Msg
addPayment apiUrl paymentType table time =
    Http.request
        { url = apiUrl ++ "/payments"
        , headers =
            [ Http.header "Access-Control-Allow-Origin" "*"
            ]
        , expect = Http.expectJson (PaymentAdded table) paymentDecoder
        , method = "POST"
        , timeout = Nothing
        , tracker = Nothing
        , body =
            Http.jsonBody (paymentTableEncoder paymentType table time)
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


update : Msg -> Model a -> ( Model a, Cmd Msg )
update msg model =
    case msg of
        AllPaymentsFetched response ->
            ( { model | payments = Result.withDefault model.payments response }, Cmd.none )

        PaymentAdded table response ->
            case response of
                Ok payment ->
                    let
                        updatedTable =
                            { table | state = HasFinishedPayment }
                    in
                    ( { model
                        | selectedTable = Just updatedTable
                        , tables = updateTableAtId updatedTable model.tables
                        , payments = Array.push payment model.payments
                      }
                    , Cmd.none
                    )

                Err _ ->
                    ( model, Cmd.none )
