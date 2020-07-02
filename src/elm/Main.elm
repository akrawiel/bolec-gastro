module Main exposing (main)

import Array exposing (Array)
import Browser exposing (Document)
import Browser.Navigation as Nav
import Entities.Drink exposing (Drink, getAllDrinks)
import Entities.Meal exposing (Meal, getAllMeals)
import Entities.Order exposing (OrderMealChange(..))
import Entities.Payment exposing (Payment, getAllPayments)
import Entities.Table exposing (Table, TableState(..), mapTableForIndex)
import Html
import Pages.Admin
import Pages.Main
import Pages.NotFound
import Pages.PaymentHistory
import Task
import Time
import Url
import Url.Parser as Parser


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
    , apiUrl : String
    , tables : List Table
    , selectedTable : Maybe Table
    , customersForTable : Int
    , meals : Array Meal
    , drinks : Array Drink
    , currentlyEditedMeal : Maybe Meal
    , currentlyEditedDrink : Maybe Drink
    , currentlyEditedName : String
    , currentlyEditedPrice : String
    , currentlyEditedVolume : String
    , addingMeal : Bool
    , addingDrink : Bool
    , payments : Array Payment
    , timeZone : Maybe Time.Zone
    }


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | MealMsg Entities.Meal.Msg
    | DrinkMsg Entities.Drink.Msg
    | PaymentMsg Entities.Payment.Msg
    | MainPageMsg Pages.Main.Msg
    | AdminPageMsg Pages.Admin.Msg
    | SetTimeZone Time.Zone


type Page
    = Main
    | Admin
    | PaymentHistory



-- INIT


init : { apiUrl : String } -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init { apiUrl } url key =
    ( { key = key
      , url = url
      , apiUrl = apiUrl
      , tables = List.map mapTableForIndex (List.range 1 16)
      , selectedTable = Nothing
      , customersForTable = 1
      , meals = Array.empty
      , drinks = Array.empty
      , currentlyEditedDrink = Nothing
      , currentlyEditedMeal = Nothing
      , currentlyEditedName = ""
      , currentlyEditedPrice = ""
      , currentlyEditedVolume = ""
      , addingMeal = False
      , addingDrink = False
      , payments = Array.empty
      , timeZone = Nothing
      }
    , Cmd.batch
        [ Cmd.map MealMsg (getAllMeals apiUrl)
        , Cmd.map DrinkMsg (getAllDrinks apiUrl)
        , Cmd.map PaymentMsg (getAllPayments apiUrl)
        , Task.perform SetTimeZone Time.here
        ]
    )



-- UPDATE


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

        MealMsg message ->
            Entities.Meal.update message model |> Tuple.mapSecond (Cmd.map MealMsg)

        DrinkMsg message ->
            Entities.Drink.update message model |> Tuple.mapSecond (Cmd.map DrinkMsg)

        PaymentMsg message ->
            Entities.Payment.update message model |> Tuple.mapSecond (Cmd.map PaymentMsg)

        MainPageMsg message ->
            Pages.Main.update message model |> Tuple.mapSecond (Cmd.map MainPageMsg)

        AdminPageMsg message ->
            Pages.Admin.update message model |> Tuple.mapSecond (Cmd.map AdminPageMsg)

        SetTimeZone timeZone ->
            ( { model | timeZone = Just timeZone }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


urlParser : Parser.Parser (Page -> a) a
urlParser =
    Parser.oneOf
        [ Parser.map Main Parser.top
        , Parser.map Admin (Parser.s "admin")
        , Parser.map PaymentHistory (Parser.s "payment-history")
        ]


view : Model -> Document Msg
view model =
    { title = "Bolec Gastro"
    , body =
        case Parser.parse urlParser model.url of
            Just Main ->
                [ Html.map MainPageMsg (Pages.Main.view model) ]

            Just Admin ->
                [ Html.map AdminPageMsg (Pages.Admin.view model) ]

            Just PaymentHistory ->
                [ Pages.PaymentHistory.view model ]

            Nothing ->
                [ Pages.NotFound.view ]
    }
