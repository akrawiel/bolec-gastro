module Main exposing (main)

import Browser exposing (Document)
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Keyed as Keyed
import Html.Lazy exposing (lazy)
import Url


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }



-- INIT


type TableState
    = Empty
    | HasCustomers Int


type alias Table =
    { name : String
    , state : TableState
    , id : Int
    }


type alias Model =
    { key : Nav.Key
    , url : Url.Url
    , tables : List Table
    , selectedTable : Maybe Table
    , customersForTable : Int
    }


mapTable : Int -> Table
mapTable index =
    { name = "Table " ++ String.fromInt index
    , id = index
    , state = Empty
    }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    ( { key = key
      , url = url
      , tables = List.map mapTable (List.range 1 16)
      , selectedTable = Nothing
      , customersForTable = 1
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | ChangeSelectedTable (Maybe Table)
    | IncrementCustomersForTable
    | DecrementCustomersForTable
    | ReserveTable Int Int
    | EmptyTable Int


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



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


getTableTileLabel : Table -> String
getTableTileLabel table =
    case table.state of
        Empty ->
            "Empty"

        HasCustomers customerCount ->
            "Customers: " ++ String.fromInt customerCount


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


viewSidePanel : Model -> Html Msg
viewSidePanel { customersForTable, selectedTable } =
    aside [ class "side-panel" ]
        [ case selectedTable of
            Just table ->
                div [ class "table-settings-container" ]
                    [ div [ class "font-size-lg" ] [ text table.name ]
                    , case table.state of
                        Empty ->
                            div []
                                [ div []
                                    [ button [ class "button", onClick DecrementCustomersForTable ] [ text "-" ]
                                    , div [] [ text (String.fromInt customersForTable) ]
                                    , button [ class "button", onClick IncrementCustomersForTable ] [ text "+" ]
                                    ]
                                , div []
                                    [ button
                                        [ class "button"
                                        , onClick
                                            (ReserveTable table.id
                                                customersForTable
                                            )
                                        ]
                                        [ text "Reserve table" ]
                                    ]
                                ]

                        HasCustomers _ ->
                            button [ class "button", onClick (EmptyTable table.id) ] [ text "Empty table" ]
                    , div []
                        [ button [ class "button", onClick (ChangeSelectedTable Nothing) ]
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
            [ viewTables model.tables
            , viewSidePanel model
            ]
        ]
    }
