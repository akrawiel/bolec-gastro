module Main exposing (main)

import Browser exposing (Document)
import Browser.Navigation as Nav
import Entities.Table exposing (mapTableForIndex, viewTables)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Types exposing (Model, Msg(..), TableState(..))
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


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    ( { key = key
      , url = url
      , tables = List.map mapTableForIndex (List.range 1 16)
      , selectedTable = Nothing
      , customersForTable = 1
      }
    , Cmd.none
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



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


viewSidePanel : Model -> Html Msg
viewSidePanel { customersForTable, selectedTable } =
    aside [ class "side-panel" ]
        [ case selectedTable of
            Just table ->
                div [ class "table-settings-container font-size-xl" ]
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
                            button [ class "button width-full mb-sm", onClick (EmptyTable table.id) ] [ text "Empty table" ]
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
            [ viewTables model.tables
            , viewSidePanel model
            ]
        ]
    }
