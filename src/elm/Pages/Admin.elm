module Pages.Admin exposing (Msg, update, view)

import Array exposing (Array)
import Entities.Drink exposing (Drink)
import Entities.Meal exposing (Meal, MealRequestMethods, mealDecoder)
import Html exposing (Html, a, b, button, div, form, input, label, span, text)
import Html.Attributes as Attr exposing (class, for, href, name, required, step, type_, value)
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
    , currentlyEditedVolume : String
    }


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


type alias MealExtendable a =
    { a
        | name : String
        , price : Float
    }


type alias NewMeal =
    MealExtendable {}


type Msg
    = ChangeEditedMeal (Maybe Meal)
    | ChangeEditedDrink (Maybe Drink)
    | ChangeEditedEntity EditedEntity
    | UpdateMeal Meal
    | MealUpdated (Result Http.Error String)
    | UpdateDrink Drink
    | DrinkUpdated (Result Http.Error String)
    | SetAddingMeal
    | AddMeal NewMeal
    | MealAdded (Result Http.Error Meal)



-- UPDATE


updateDrinkRequest : Model a -> Drink -> Cmd Msg
updateDrinkRequest { apiUrl } drink =
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

        ChangeEditedDrink drink ->
            ( { model
                | currentlyEditedDrink = drink
                , currentlyEditedMeal = Nothing
                , currentlyEditedName = drink |> Maybe.map .name |> Maybe.withDefault ""
                , currentlyEditedPrice = drink |> Maybe.map (.price >> Round.floor 2) |> Maybe.withDefault ""
                , currentlyEditedVolume = drink |> Maybe.map (.volume >> String.fromInt) |> Maybe.withDefault ""
                , addingMeal = False
                , addingDrink = False
              }
            , Cmd.none
            )

        ChangeEditedEntity { currentlyEditedName, currentlyEditedPrice, currentlyEditedVolume } ->
            ( { model
                | currentlyEditedName = currentlyEditedName
                , currentlyEditedPrice = currentlyEditedPrice
                , currentlyEditedVolume = currentlyEditedVolume
              }
            , Cmd.none
            )

        UpdateDrink drink ->
            let
                currentlyEditedDrink =
                    { drink
                        | name = model.currentlyEditedName
                        , price =
                            model.currentlyEditedPrice
                                |> String.toFloat
                                |> Maybe.withDefault drink.price
                        , volume =
                            model.currentlyEditedVolume
                                |> String.toInt
                                |> Maybe.withDefault drink.volume
                    }
            in
            ( { model
                | currentlyEditedDrink = Just currentlyEditedDrink
              }
            , updateDrinkRequest model currentlyEditedDrink
            )

        DrinkUpdated response ->
            ( case response of
                Ok _ ->
                    { model | drinks = Array.map (replaceEditedDrinkData model.currentlyEditedDrink) model.drinks }

                Err _ ->
                    model
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

        AddMeal _ ->
            let
                newMeal =
                    case
                        ( model.currentlyEditedName
                        , model.currentlyEditedPrice |> String.toFloat
                        )
                    of
                        ( name, Just price ) ->
                            Just
                                { name = name
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



-- VIEW


changeEditedName : EditedEntity -> String -> Msg
changeEditedName editedEntity value =
    ChangeEditedEntity { editedEntity | currentlyEditedName = value }


changeEditedPrice : EditedEntity -> String -> Msg
changeEditedPrice editedEntity value =
    ChangeEditedEntity { editedEntity | currentlyEditedPrice = value }


changeEditedVolume : EditedEntity -> String -> Msg
changeEditedVolume editedEntity value =
    ChangeEditedEntity { editedEntity | currentlyEditedVolume = value }


viewEditedDrink : (Drink -> Msg) -> Model a -> Drink -> Html Msg
viewEditedDrink onDrinkSubmit { currentlyEditedName, currentlyEditedPrice, currentlyEditedVolume } drink =
    let
        editedEntity =
            { currentlyEditedName = currentlyEditedName
            , currentlyEditedPrice = currentlyEditedPrice
            , currentlyEditedVolume = currentlyEditedVolume
            }
    in
    div [ class "flex-1 pl-md" ]
        [ form [ class "color-light font-size-lg", onSubmit (onDrinkSubmit drink) ]
            [ div [ class "mb-md" ]
                [ span []
                    [ text "Editing " ]
                , case drink.name of
                    "" ->
                        span [] [ text "drink" ]

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
            , div [ class "mb-sm" ]
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
            , div [ class "mb-lg" ]
                [ label [ class "font-size-md font-weight-700", for "volume" ] [ text "Volume (in ml)" ]
                , input
                    [ onInput (changeEditedVolume editedEntity)
                    , class "width-full sizing-border"
                    , name "volume"
                    , type_ "number"
                    , step "1"
                    , Attr.min "1"
                    , value currentlyEditedVolume
                    , required True
                    ]
                    []
                ]
            , div [ class "mb-md" ]
                [ button [ class "button width-full", type_ "submit" ] [ text "Submit" ]
                ]
            , div []
                [ button [ class "button width-full", onClick (ChangeEditedDrink Nothing) ] [ text "Cancel" ]
                ]
            ]
        ]


viewEditedMeal : (MealExtendable b -> Msg) -> Model a -> MealExtendable b -> Html Msg
viewEditedMeal onMealSubmit { addingMeal, currentlyEditedName, currentlyEditedPrice } meal =
    let
        editedEntity =
            { currentlyEditedName = currentlyEditedName
            , currentlyEditedPrice = currentlyEditedPrice
            , currentlyEditedVolume = ""
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
                [ button [ class "button width-full", onClick (ChangeEditedMeal Nothing) ] [ text "Cancel" ]
                ]
            ]
        ]


viewEditedEntry : Model a -> Html Msg
viewEditedEntry model =
    case ( model.addingMeal, model.currentlyEditedMeal, model.currentlyEditedDrink ) of
        ( True, _, _ ) ->
            viewEditedMeal AddMeal model { name = "", price = 0 }

        ( False, Just meal, Nothing ) ->
            viewEditedMeal UpdateMeal model meal

        ( False, Nothing, Just drink ) ->
            viewEditedDrink UpdateDrink model drink

        _ ->
            div [ class "flex-1 color-light font-size-xl text-center pl-md" ] [ text "No entry selected" ]


viewDrink : Drink -> Html Msg
viewDrink drink =
    div [ class "flex align-center row color-light mb-sm" ]
        [ div [] [ text (drink.name ++ " " ++ String.fromInt drink.volume ++ "ml") ]
        , div [ class "leaders flex-1 height-sm mx-xs" ] []
        , div [] [ text (Round.floor 2 drink.price) ]
        , button [ class "button ml-sm", onClick (ChangeEditedDrink (Just drink)) ] [ text "Edit" ]
        ]


viewKeyedDrinks : Drink -> ( String, Html Msg )
viewKeyedDrinks drink =
    ( String.fromInt drink.id, lazy viewDrink drink )


viewDrinks : Array Drink -> Html Msg
viewDrinks drinks =
    div []
        [ div [ class "color-light font-size-lg my-md font-style-italic" ] [ text "Drinks" ]
        , Keyed.node "div"
            []
            (drinks
                |> Array.toList
                |> List.sortBy .name
                |> List.map viewKeyedDrinks
            )
        ]


viewMeal : Meal -> Html Msg
viewMeal meal =
    div [ class "flex align-center row color-light mb-sm" ]
        [ div [] [ text meal.name ]
        , div [ class "leaders flex-1 height-sm mx-xs" ] []
        , div [] [ text (Round.floor 2 meal.price) ]
        , button [ class "button ml-sm", onClick (ChangeEditedMeal (Just meal)) ] [ text "Edit" ]
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


view : Model a -> Html Msg
view model =
    div [ class "bg-secondary height-full py-md px-md sizing-border" ]
        [ div [ class "mb-md flex justify-between" ]
            [ div [ class "font-size-xl font-weight-700 color-light" ]
                [ text "Bolec Gastro" ]
            , a
                [ class "button font-size-lg", href "/" ]
                [ text "Home" ]
            ]
        , div [ class "flex row" ]
            [ div [ class "flex-1" ]
                [ viewMeals model.meals
                , viewDrinks model.drinks
                ]
            , viewEditedEntry model
            ]
        ]
