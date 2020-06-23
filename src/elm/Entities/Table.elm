module Entities.Table exposing (mapTableForIndex, viewTables)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Keyed as Keyed
import Html.Lazy exposing (lazy)
import Types exposing (Msg(..), Table, TableState(..))



-- HELPERS


mapTableForIndex : Int -> Table
mapTableForIndex index =
    { name = "Table " ++ String.fromInt index
    , id = index
    , state = Empty
    }


getTableTileLabel : Table -> String
getTableTileLabel table =
    case table.state of
        Empty ->
            "Empty"

        HasCustomers customerCount ->
            "Customers: " ++ String.fromInt customerCount



-- VIEW HELPERS


viewTable : Table -> Html Msg
viewTable table =
    button [ class "button table-tile", onClick (ChangeSelectedTable (Just table)) ]
        [ div [ class "font-size-lg" ] [ text table.name ]
        , div [ class "font-size-md" ] [ text (getTableTileLabel table) ]
        ]


viewKeyedTable : Table -> ( String, Html Msg )
viewKeyedTable table =
    ( String.fromInt table.id, lazy viewTable table )


viewTables : List Table -> Html Msg
viewTables tables =
    Keyed.node "div" [ class "table-grid" ] (List.map viewKeyedTable tables)
