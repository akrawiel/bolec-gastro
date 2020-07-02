module Entities.Meal exposing
    ( Meal
    , MealExtendable
    , Msg
    , NewMeal
    , addMeal
    , getAllMeals
    , mealArrayDecoder
    , mealDecoder
    , removeMeal
    , update
    , updateMeal
    , viewMeals
    )

import Array exposing (Array)
import Html exposing (Html, button, div, text)
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


type alias MealExtendable a =
    { a
        | name : String
        , price : Float
    }


type alias NewMeal =
    MealExtendable {}


type alias Meal =
    { id : Int
    , name : String
    , price : Float
    }


type alias Model a =
    { a
        | meals : Array Meal
        , currentlyEditedMeal : Maybe Meal
        , addingMeal : Bool
    }


type Msg
    = AllMealsFetched (Result Http.Error (Array Meal))
    | MealUpdated (Result Http.Error String)
    | MealAdded (Result Http.Error Meal)
    | MealRemoved Int (Result Http.Error ())



-- JSON DECODERS


mealDecoder : Decoder Meal
mealDecoder =
    succeed Meal
        |> required "id" int
        |> required "name" string
        |> required "price" float


mealArrayDecoder : Decoder (Array Meal)
mealArrayDecoder =
    array mealDecoder



-- REQUESTS


getAllMeals : String -> Cmd Msg
getAllMeals apiUrl =
    Http.request
        { url = apiUrl ++ "/meals"
        , headers =
            [ Http.header "Access-Control-Allow-Origin" "*"
            ]
        , expect = Http.expectJson AllMealsFetched mealArrayDecoder
        , method = "GET"
        , timeout = Nothing
        , tracker = Nothing
        , body = Http.emptyBody
        }


updateMeal : String -> Meal -> Cmd Msg
updateMeal apiUrl meal =
    Http.request
        { url = apiUrl ++ "/meals/" ++ String.fromInt meal.id
        , headers =
            [ Http.header "Access-Control-Allow-Origin" "*"
            ]
        , expect = Http.expectString MealUpdated
        , method = "PUT"
        , timeout = Nothing
        , tracker = Nothing
        , body =
            Http.jsonBody
                (Encode.object
                    [ ( "id", Encode.int meal.id )
                    , ( "name", Encode.string meal.name )
                    , ( "price", Encode.float meal.price )
                    ]
                )
        }


addMeal : String -> NewMeal -> Cmd Msg
addMeal apiUrl newMeal =
    Http.request
        { url = apiUrl ++ "/meals"
        , headers =
            [ Http.header "Access-Control-Allow-Origin" "*"
            ]
        , expect = Http.expectJson MealAdded mealDecoder
        , method = "POST"
        , timeout = Nothing
        , tracker = Nothing
        , body =
            Http.jsonBody
                (Encode.object
                    [ ( "name", Encode.string newMeal.name )
                    , ( "price", Encode.float newMeal.price )
                    ]
                )
        }


removeMeal : String -> Meal -> Cmd Msg
removeMeal apiUrl { id } =
    Http.request
        { url = apiUrl ++ "/meals/" ++ String.fromInt id
        , headers =
            [ Http.header "Access-Control-Allow-Origin" "*"
            ]
        , expect = Http.expectWhatever (MealRemoved id)
        , method = "DELETE"
        , timeout = Nothing
        , tracker = Nothing
        , body =
            Http.emptyBody
        }



-- UPDATE


replaceEditedMealData : Maybe Meal -> Meal -> Meal
replaceEditedMealData currentlyEditedMeal meal =
    case currentlyEditedMeal of
        Just newMeal ->
            if meal.id == newMeal.id then
                newMeal

            else
                meal

        Nothing ->
            meal


filterExistingMeals : Int -> Array Meal -> Array Meal
filterExistingMeals removedId meals =
    Array.filter (\meal -> meal.id /= removedId) meals


update : Msg -> Model a -> ( Model a, Cmd Msg )
update msg model =
    case msg of
        AllMealsFetched response ->
            ( { model | meals = Result.withDefault model.meals response }, Cmd.none )

        MealAdded response ->
            case response of
                Ok meal ->
                    ( { model
                        | meals = Array.push meal model.meals
                        , currentlyEditedMeal = Just meal
                        , addingMeal = False
                      }
                    , Cmd.none
                    )

                Err _ ->
                    ( model, Cmd.none )

        MealUpdated response ->
            ( case response of
                Ok _ ->
                    { model | meals = Array.map (replaceEditedMealData model.currentlyEditedMeal) model.meals }

                Err _ ->
                    model
            , Cmd.none
            )

        MealRemoved removedId response ->
            case response of
                Ok _ ->
                    case model.currentlyEditedMeal of
                        Just editedMeal ->
                            ( { model
                                | meals = model.meals |> filterExistingMeals removedId
                                , currentlyEditedMeal =
                                    if editedMeal.id == removedId then
                                        Nothing

                                    else
                                        Just editedMeal
                              }
                            , Cmd.none
                            )

                        Nothing ->
                            ( { model
                                | meals = model.meals |> filterExistingMeals removedId
                              }
                            , Cmd.none
                            )

                Err _ ->
                    ( model, Cmd.none )



-- VIEW


viewMeal : (Int -> msg) -> (Int -> msg) -> Meal -> Html msg
viewMeal onIncrementClick onDecrementClick meal =
    div [ class "meals__container gap-md mb-md" ]
        [ div [ class "meals__card p-sm border-radius-sm" ]
            [ div [ class "font-size-lg" ] [ text meal.name ]
            , div [ class "font-size-sm" ] [ text (Round.floor 2 meal.price) ]
            ]
        , button [ class "button p-none", onClick (onDecrementClick meal.id) ] [ text "-" ]
        , button [ class "button p-none", onClick (onIncrementClick meal.id) ] [ text "+" ]
        ]


viewKeyedMeals : (Int -> msg) -> (Int -> msg) -> Meal -> ( String, Html msg )
viewKeyedMeals onIncrementClick onDecrementClick meal =
    ( String.fromInt meal.id, lazy3 viewMeal onIncrementClick onDecrementClick meal )


viewMeals : (Int -> msg) -> (Int -> msg) -> Array Meal -> Html msg
viewMeals onIncrementClick onDecrementClick meals =
    div [ class "flex column" ]
        [ div [ class "mb-sm font-size-lg text-center" ] [ text "Meals" ]
        , Keyed.node "div"
            []
            (meals
                |> Array.toList
                |> List.sortBy .name
                |> List.map (viewKeyedMeals onIncrementClick onDecrementClick)
            )
        ]
