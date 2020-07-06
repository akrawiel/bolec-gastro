module Pages.Admin.Meals exposing (Msg, update, viewAddOrEditMeal, viewMeals)

import Array exposing (Array)
import Entities.Drink exposing (Drink)
import Entities.Meal exposing (Meal, MealExtendable, NewMeal, addMeal, removeMeal, updateMeal)
import Html exposing (Html, b, button, div, form, input, label, span, text)
import Html.Attributes as Attr exposing (class, for, name, required, step, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Html.Keyed as Keyed
import Html.Lazy exposing (lazy2)
import Round



-- TYPES


type alias EditedEntity =
    { currentlyEditedName : String
    , currentlyEditedPrice : String
    }


type Msg
    = ChangeEditedMeal (Maybe Meal)
    | ChangeEditedEntity EditedEntity
    | UpdateMeal Meal
    | SetAddingMeal
    | AddMeal NewMeal
    | RemoveMeal Meal
    | MealRequestMsg Entities.Meal.Msg


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
            , Cmd.map MealRequestMsg (updateMeal model.apiUrl currentlyEditedMeal)
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
                    ( model, Cmd.map MealRequestMsg (addMeal model.apiUrl meal) )

                Nothing ->
                    ( model, Cmd.none )

        RemoveMeal meal ->
            ( model, Cmd.map MealRequestMsg (removeMeal model.apiUrl meal) )

        MealRequestMsg message ->
            Entities.Meal.update message model |> Tuple.mapSecond (Cmd.map MealRequestMsg)



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
    div [ class "flex-1 p-md" ]
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
            , div [ class "mb-md flex" ]
                [ button [ class "button width-full mr-sm", type_ "submit" ] [ text "Submit" ]
                , button [ class "button width-full ml-sm", type_ "button", onClick (ChangeEditedMeal Nothing) ] [ text "Cancel" ]
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


viewMeal : Maybe Meal -> Meal -> Html Msg
viewMeal currentlyEditedMeal meal =
    let
        mealHighlightClass =
            case currentlyEditedMeal of
                Just { id } ->
                    if id == meal.id then
                        "font-weight-700 text-shadow-error"

                    else
                        ""

                Nothing ->
                    ""
    in
    div [ class "flex align-center row color-light mb-sm" ]
        [ div [ class mealHighlightClass ] [ text meal.name ]
        , div [ class "leaders flex-1 height-sm mx-xs" ] []
        , div [ class mealHighlightClass ] [ text (Round.floor 2 meal.price) ]
        , button [ class "button ml-sm", onClick (ChangeEditedMeal (Just meal)) ] [ text "Edit" ]
        , button [ class "button ml-sm", onClick (RemoveMeal meal) ] [ text "×" ]
        ]


viewKeyedMeals : Maybe Meal -> Meal -> ( String, Html Msg )
viewKeyedMeals currentlyEditedMeal meal =
    ( String.fromInt meal.id, lazy2 viewMeal currentlyEditedMeal meal )


viewMeals : Model a -> Html Msg
viewMeals { meals, currentlyEditedMeal } =
    div []
        [ div [ class "color-light font-size-lg mb-md font-style-italic" ] [ text "Meals" ]
        , Keyed.node "div"
            []
            (meals
                |> Array.toList
                |> List.sortBy .name
                |> List.map (viewKeyedMeals currentlyEditedMeal)
            )
        , button [ class "button width-full font-size-lg", onClick SetAddingMeal ] [ text "+ Add meal" ]
        ]
