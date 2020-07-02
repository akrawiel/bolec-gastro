module Entities.Table exposing (Table, TableState(..), mapTableForIndex, viewTables)

import Entities.Order as Order exposing (Order)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Keyed as Keyed
import Html.Lazy exposing (lazy)



-- TYPES


type TableState
    = Empty
    | HasCustomers
    | HasStartedPayment
    | HasFinishedPayment


type alias Table =
    { name : String
    , state : TableState
    , id : Int
    , order : Order
    , customerCount : Int
    }



-- HELPERS


mapTableForIndex : Int -> Table
mapTableForIndex index =
    { name = "Table " ++ String.fromInt index
    , id = index
    , state = Empty
    , order = Order.empty
    , customerCount = 0
    }


getTableTileLabel : Table -> String
getTableTileLabel table =
    case table.state of
        Empty ->
            "Empty"

        HasCustomers ->
            "Customers: " ++ String.fromInt table.customerCount

        HasStartedPayment ->
            "Started payment"

        HasFinishedPayment ->
            "Finished"



-- VIEW HELPERS


viewTable : (Table -> msg) -> Table -> Html msg
viewTable onTableClick table =
    button [ class "button table-tile", onClick (onTableClick table) ]
        [ div [ class "font-size-lg" ] [ text table.name ]
        , div [ class "font-size-md" ] [ text (getTableTileLabel table) ]
        ]


viewKeyedTable : (Table -> msg) -> Table -> ( String, Html msg )
viewKeyedTable onTableClick table =
    ( String.fromInt table.id, lazy (viewTable onTableClick) table )


viewTables : (Table -> msg) -> List Table -> Html msg
viewTables onTableClick tables =
    Keyed.node "div" [ class "table-grid" ] (List.map (viewKeyedTable onTableClick) tables)
