module Main exposing (main)

import Array exposing (Array)
import Browser exposing (Document)
import Browser.Navigation as Nav
import Entities.Drink
    exposing
        ( Drink
        , DrinkRequestMsg
        , getAllDrinks
        , updateDrinks
        )
import Entities.Meal
    exposing
        ( Meal
        , MealRequestMsg
        , getAllMeals
        , updateMeals
        )
import Entities.Order exposing (OrderMealChange(..))
import Entities.Table exposing (Table, TableState(..), mapTableForIndex)
import Html
import Pages.Admin
import Pages.Main
import Pages.NotFound
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
    }


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | MealMsg MealRequestMsg
    | DrinkMsg DrinkRequestMsg
    | MainPageMsg Pages.Main.Msg
    | AdminPageMsg Pages.Admin.Msg


type Page
    = Main
    | Admin



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
      }
    , Cmd.batch
        [ Cmd.map MealMsg (getAllMeals apiUrl)
        , Cmd.map DrinkMsg (getAllDrinks apiUrl)
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

        MealMsg requestMessage ->
            ( { model | meals = updateMeals requestMessage model.meals }, Cmd.none )

        DrinkMsg requestMessage ->
            ( { model | drinks = updateDrinks requestMessage model.drinks }, Cmd.none )

        MainPageMsg message ->
            Pages.Main.update message model |> Tuple.mapSecond (Cmd.map MainPageMsg)

        AdminPageMsg message ->
            Pages.Admin.update message model |> Tuple.mapSecond (Cmd.map AdminPageMsg)



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

            Nothing ->
                [ Pages.NotFound.view ]
    }
