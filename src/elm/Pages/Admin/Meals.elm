module Pages.Admin.Meals exposing (Msg, update, viewAddOrEditMeal, viewMeals)

import Array exposing (Array)
import Entities.Drink exposing (Drink)
import Entities.Meal exposing (Meal, mealDecoder)
import Html exposing (Html, b, button, div, form, input, label, span, text)
import Html.Attributes as Attr exposing (class, for, name, required, step, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Html.Keyed as Keyed
import Html.Lazy exposing (lazy)
import Http
import Json.Encode as Encode
import Round



-- TYPES


type alias EditedEntity =
    { currentlyEditedName : String
    , currentlyEditedPrice : String
    }


type alias MealExtendable a =
    { a
        | name : String
        , price : Float
    }


type alias NewMeal =
    MealExtendable {}


type Msg
    = ChangeEditedMeal (Maybe Meal)
    | ChangeEditedEntity EditedEntity
    | UpdateMeal Meal
    | MealUpdated (Result Http.Error String)
    | SetAddingMeal
    | AddMeal NewMeal
    | MealAdded (Result Http.Error Meal)
    | RemoveMeal Meal
    | MealRemoved Int (Result Http.Error ())


type alias Model a =
    { a
        | apiUrl : String
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



-- UPDATE


updateMealRequest : Model a -> Meal -> Cmd Msg
updateMealRequest { apiUrl } meal =
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


addMealRequest : Model a -> NewMeal -> Cmd Msg
addMealRequest { apiUrl } newMeal =
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


removeMealRequest : Model a -> Meal -> Cmd Msg
removeMealRequest { apiUrl } { id } =
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


filterExistingMeals : Int -> Array Meal -> Array Meal
filterExistingMeals removedId meals =
    Array.filter (\meal -> meal.id /= removedId) meals


update : Msg -> Model a -> ( Model a, Cmd Msg )
update msg model =
    case msg of
        ChangeEditedMeal meal ->
            ( { model
                | currentlyEditedMeal = meal
                , currentlyEditedDrink = Nothing
                , currentlyEditedName = meal |> Maybe.map .name |> Maybe.withDefault ""
                , currentlyEditedPrice = meal |> Maybe.map (.price >> Round.floor 2) |> Maybe.withDefault ""
                , addingMeal = False
                , addingDrink = False
              }
            , Cmd.none
            )

        ChangeEditedEntity { currentlyEditedName, currentlyEditedPrice } ->
            ( { model
                | currentlyEditedName = currentlyEditedName
                , currentlyEditedPrice = currentlyEditedPrice
              }
            , Cmd.none
            )

        UpdateMeal meal ->
            let
                currentlyEditedMeal =
                    { meal
                        | name = model.currentlyEditedName
                        , price =
                            model.currentlyEditedPrice
                                |> String.toFloat
                                |> Maybe.withDefault meal.price
                    }
            in
            ( { model
                | currentlyEditedMeal = Just currentlyEditedMeal
              }
            , updateMealRequest model currentlyEditedMeal
            )

        MealUpdated response ->
            ( case response of
                Ok _ ->
                    { model | meals = Array.map (replaceEditedMealData model.currentlyEditedMeal) model.meals }

                Err _ ->
                    model
            , Cmd.none
            )

        SetAddingMeal ->
            ( { model
                | currentlyEditedMeal = Nothing
                , currentlyEditedDrink = Nothing
                , addingMeal = True
                , addingDrink = False
                , currentlyEditedName = ""
                , currentlyEditedPrice = ""
              }
            , Cmd.none
            )

        AddMeal addedMeal ->
            let
                newMeal =
                    case
                        ( model.currentlyEditedName
                        , model.currentlyEditedPrice |> String.toFloat
                        )
                    of
                        ( name, Just price ) ->
                            Just
                                { addedMeal
                                    | name = name
                                    , price = price
                                }

                        _ ->
                            Nothing
            in
            case newMeal of
                Just meal ->
                    ( model, addMealRequest model meal )

                Nothing ->
                    ( model, Cmd.none )

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

        RemoveMeal meal ->
            ( model, removeMealRequest model meal )

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



-- EVENTS


changeEditedName : EditedEntity -> String -> Msg
changeEditedName editedEntity value =
    ChangeEditedEntity { editedEntity | currentlyEditedName = value }


changeEditedPrice : EditedEntity -> String -> Msg
changeEditedPrice editedEntity value =
    ChangeEditedEntity { editedEntity | currentlyEditedPrice = value }



-- VIEW


viewMealForm : (MealExtendable b -> Msg) -> Model a -> MealExtendable b -> Html Msg
viewMealForm onMealSubmit { addingMeal, currentlyEditedName, currentlyEditedPrice } meal =
    let
        editedEntity =
            { currentlyEditedName = currentlyEditedName
            , currentlyEditedPrice = currentlyEditedPrice
            }
    in
    div [ class "flex-1 pl-md" ]
        [ form [ class "color-light font-size-lg", onSubmit (onMealSubmit meal) ]
            [ div [ class "mb-md" ]
                [ span []
                    [ text
                        (if addingMeal then
                            "Adding "

                         else
                            "Editing "
                        )
                    ]
                , case meal.name of
                    "" ->
                        span [] [ text "meal" ]

                    name ->
                        b [] [ text ("\"" ++ name ++ "\"") ]
                ]
            , div [ class "mb-sm" ]
                [ label [ class "font-size-md font-weight-700", for "name" ] [ text "Name" ]
                , input
                    [ onInput (changeEditedName editedEntity)
                    , value currentlyEditedName
                    , class "width-full sizing-border"
                    , name "name"
                    , required True
                    ]
                    []
                ]
            , div [ class "mb-lg" ]
                [ label [ class "font-size-md font-weight-700", for "price" ] [ text "Price" ]
                , input
                    [ onInput (changeEditedPrice editedEntity)
                    , class "width-full sizing-border"
                    , name "price"
                    , type_ "number"
                    , step "0.01"
                    , Attr.min "0.1"
                    , value currentlyEditedPrice
                    , required True
                    ]
                    []
                ]
            , div [ class "mb-md" ]
                [ button [ class "button width-full", type_ "submit" ] [ text "Submit" ]
                ]
            , div []
                [ button [ class "button width-full", type_ "button", onClick (ChangeEditedMeal Nothing) ] [ text "Cancel" ]
                ]
            ]
        ]


viewAddOrEditMeal : Model a -> Html Msg
viewAddOrEditMeal model =
    case ( model.addingMeal, model.currentlyEditedMeal ) of
        ( True, _ ) ->
            viewMealForm AddMeal model { name = "", price = 0 }

        ( False, Just meal ) ->
            viewMealForm UpdateMeal model meal

        _ ->
            text ""


viewMeal : Meal -> Html Msg
viewMeal meal =
    div [ class "flex align-center row color-light mb-sm" ]
        [ div [] [ text meal.name ]
        , div [ class "leaders flex-1 height-sm mx-xs" ] []
        , div [] [ text (Round.floor 2 meal.price) ]
        , button [ class "button ml-sm", onClick (ChangeEditedMeal (Just meal)) ] [ text "Edit" ]
        , button [ class "button ml-sm", onClick (RemoveMeal meal) ] [ text "×" ]
        ]


viewKeyedMeals : Meal -> ( String, Html Msg )
viewKeyedMeals meal =
    ( String.fromInt meal.id, lazy viewMeal meal )


viewMeals : Array Meal -> Html Msg
viewMeals meals =
    div []
        [ div [ class "color-light font-size-lg mb-md font-style-italic" ] [ text "Meals" ]
        , Keyed.node "div"
            []
            (meals
                |> Array.toList
                |> List.sortBy .name
                |> List.map viewKeyedMeals
            )
        , button [ class "button width-full font-size-lg", onClick SetAddingMeal ] [ text "+ Add meal" ]
        ]
