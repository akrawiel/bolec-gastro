module Entities.Drink exposing
    ( Drink
    , DrinkExtendable
    , Msg
    , NewDrink
    , addDrink
    , drinkArrayDecoder
    , drinkDecoder
    , getAllDrinks
    , removeDrink
    , update
    , updateDrink
    , viewDrinks
    )

import Array exposing (Array)
import Html exposing (Html, button, div, span, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Html.Keyed as Keyed
import Html.Lazy exposing (lazy3)
import Http
import Json.Decode exposing (Decoder, array, float, int, string, succeed)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode
import Round



-- TYPES


type alias DrinkExtendable a =
    { a
        | name : String
        , price : Float
        , volume : Int
    }


type alias NewDrink =
    DrinkExtendable {}


type alias Drink =
    { id : Int
    , name : String
    , price : Float
    , volume : Int
    }


type alias Model a =
    { a
        | drinks : Array Drink
        , currentlyEditedDrink : Maybe Drink
        , addingDrink : Bool
    }


type Msg
    = AllDrinksFetched (Result Http.Error (Array Drink))
    | DrinkAdded (Result Http.Error Drink)
    | DrinkUpdated (Result Http.Error String)
    | DrinkRemoved Int (Result Http.Error ())



-- JSON DECODERS


drinkDecoder : Decoder Drink
drinkDecoder =
    succeed Drink
        |> required "id" int
        |> required "name" string
        |> required "price" float
        |> required "volume" int


drinkArrayDecoder : Decoder (Array Drink)
drinkArrayDecoder =
    array drinkDecoder



-- REQUESTS


getAllDrinks : String -> Cmd Msg
getAllDrinks apiUrl =
    Http.request
        { url = apiUrl ++ "/drinks"
        , headers =
            [ Http.header "Access-Control-Allow-Origin" "*"
            ]
        , expect = Http.expectJson AllDrinksFetched drinkArrayDecoder
        , method = "GET"
        , timeout = Nothing
        , tracker = Nothing
        , body = Http.emptyBody
        }


updateDrink : String -> Drink -> Cmd Msg
updateDrink apiUrl drink =
    Http.request
        { url = apiUrl ++ "/drinks/" ++ String.fromInt drink.id
        , headers =
            [ Http.header "Access-Control-Allow-Origin" "*"
            ]
        , expect = Http.expectString DrinkUpdated
        , method = "PUT"
        , timeout = Nothing
        , tracker = Nothing
        , body =
            Http.jsonBody
                (Encode.object
                    [ ( "id", Encode.int drink.id )
                    , ( "name", Encode.string drink.name )
                    , ( "price", Encode.float drink.price )
                    , ( "volume", Encode.int drink.volume )
                    ]
                )
        }


addDrink : String -> NewDrink -> Cmd Msg
addDrink apiUrl newDrink =
    Http.request
        { url = apiUrl ++ "/drinks"
        , headers =
            [ Http.header "Access-Control-Allow-Origin" "*"
            ]
        , expect = Http.expectJson DrinkAdded drinkDecoder
        , method = "POST"
        , timeout = Nothing
        , tracker = Nothing
        , body =
            Http.jsonBody
                (Encode.object
                    [ ( "name", Encode.string newDrink.name )
                    , ( "price", Encode.float newDrink.price )
                    , ( "volume", Encode.int newDrink.volume )
                    ]
                )
        }


removeDrink : String -> Drink -> Cmd Msg
removeDrink apiUrl { id } =
    Http.request
        { url = apiUrl ++ "/drinks/" ++ String.fromInt id
        , headers =
            [ Http.header "Access-Control-Allow-Origin" "*"
            ]
        , expect = Http.expectWhatever (DrinkRemoved id)
        , method = "DELETE"
        , timeout = Nothing
        , tracker = Nothing
        , body =
            Http.emptyBody
        }



-- UPDATE


replaceEditedDrinkData : Maybe Drink -> Drink -> Drink
replaceEditedDrinkData currentlyEditedDrink drink =
    case currentlyEditedDrink of
        Just newDrink ->
            if drink.id == newDrink.id then
                newDrink

            else
                drink

        Nothing ->
            drink


filterExistingDrinks : Int -> Array Drink -> Array Drink
filterExistingDrinks removedId drinks =
    Array.filter (\drink -> drink.id /= removedId) drinks


update : Msg -> Model a -> ( Model a, Cmd Msg )
update msg model =
    case msg of
        AllDrinksFetched response ->
            ( { model | drinks = Result.withDefault model.drinks response }, Cmd.none )

        DrinkAdded response ->
            case response of
                Ok drink ->
                    ( { model
                        | drinks = Array.push drink model.drinks
                        , currentlyEditedDrink = Just drink
                        , addingDrink = False
                      }
                    , Cmd.none
                    )

                Err _ ->
                    ( model, Cmd.none )

        DrinkUpdated response ->
            ( case response of
                Ok _ ->
                    { model | drinks = Array.map (replaceEditedDrinkData model.currentlyEditedDrink) model.drinks }

                Err _ ->
                    model
            , Cmd.none
            )

        DrinkRemoved removedId response ->
            case response of
                Ok _ ->
                    case model.currentlyEditedDrink of
                        Just editedDrink ->
                            ( { model
                                | drinks = model.drinks |> filterExistingDrinks removedId
                                , currentlyEditedDrink =
                                    if editedDrink.id == removedId then
                                        Nothing

                                    else
                                        Just editedDrink
                              }
                            , Cmd.none
                            )

                        Nothing ->
                            ( { model
                                | drinks = model.drinks |> filterExistingDrinks removedId
                              }
                            , Cmd.none
                            )

                Err _ ->
                    ( model, Cmd.none )



-- VIEW


viewDrink : (Int -> msg) -> (Int -> msg) -> Drink -> Html msg
viewDrink onIncrementClick onDecrementClick drink =
    div [ class "meals__container gap-md mb-md" ]
        [ div [ class "meals__card p-sm border-radius-sm" ]
            [ div [ class "font-size-lg" ] [ text drink.name ]
            , div [ class "font-size-sm" ]
                [ span [] [ text (String.fromInt drink.volume ++ "ml / ") ]
                , span []
                    [ text (Round.floor 2 drink.price)
                    ]
                ]
            ]
        , button [ class "button p-none", onClick (onDecrementClick drink.id) ] [ text "-" ]
        , button [ class "button p-none", onClick (onIncrementClick drink.id) ] [ text "+" ]
        ]


viewKeyedDrinks : (Int -> msg) -> (Int -> msg) -> Drink -> ( String, Html msg )
viewKeyedDrinks onIncrementClick onDecrementClick drink =
    ( String.fromInt drink.id, lazy3 viewDrink onIncrementClick onDecrementClick drink )


viewDrinks : (Int -> msg) -> (Int -> msg) -> Array Drink -> Html msg
viewDrinks onIncrementClick onDecrementClick drinks =
    div [ class "flex column" ]
        [ div [ class "mb-sm font-size-lg text-center" ] [ text "Drinks" ]
        , Keyed.node "div"
            []
            (drinks
                |> Array.toList
                |> List.sortBy .name
                |> List.map
                    (viewKeyedDrinks onIncrementClick
                        onDecrementClick
                    )
            )
        ]
