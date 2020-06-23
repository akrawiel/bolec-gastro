module Types exposing (..)

import Browser
import Browser.Navigation as Nav
import Url



-- STATE


type alias Model =
    { key : Nav.Key
    , url : Url.Url
    , tables : List Table
    , selectedTable : Maybe Table
    , customersForTable : Int
    }


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | ChangeSelectedTable (Maybe Table)
    | IncrementCustomersForTable
    | DecrementCustomersForTable
    | ReserveTable Int Int
    | EmptyTable Int



-- TABLE


type TableState
    = Empty
    | HasCustomers Int


type alias Table =
    { name : String
    , state : TableState
    , id : Int
    }
