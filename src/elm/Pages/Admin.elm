module Pages.Admin exposing (Msg, update, view)

import Array exposing (Array)
import Entities.Drink exposing (Drink)
import Entities.Meal exposing (Meal, MealRequestMethods)
import Html exposing (Html, a, b, button, div, form, input, label, span, text)
import Html.Attributes as Attr exposing (class, for, href, name, required, step, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Html.Keyed as Keyed
import Html.Lazy exposing (lazy)
import Http
import Json.Encode as Encode
import Round



-- TYPES


type alias Model a =
    { a
        | apiUrl : String
        , meals : Array Meal
        , drinks : Array Drink
        , currentlyEditedMeal : Maybe Meal
        , currentlyEditedDrink : Maybe Drink
        , mealRequester : MealRequestMethods
    }


type Msg
    = ChangeEditedMeal (Maybe Meal)
    | ChangeEditedDrink (Maybe Drink)
    | UpdateMeal Meal
    | MealUpdated (Result Http.Error String)



-- | UpdateDrink Drink
-- | DrinkUpdated (Result Http.Error String)
-- UPDATE


updateMealRequest : String -> Meal -> Cmd Msg
updateMealRequest apiUrl meal =
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


update : Msg -> Model a -> ( Model a, Cmd Msg )
update msg model =
    case msg of
        ChangeEditedMeal meal ->
            ( { model | currentlyEditedMeal = meal, currentlyEditedDrink = Nothing }, Cmd.none )

        ChangeEditedDrink drink ->
            ( { model | currentlyEditedDrink = drink, currentlyEditedMeal = Nothing }, Cmd.none )

        UpdateMeal meal ->
            ( model
            , updateMealRequest model.apiUrl meal
            )

        MealUpdated response ->
            ( case response of
                Ok _ ->
                    { model | meals = Array.map (replaceEditedMealData model.currentlyEditedMeal) model.meals }

                Err _ ->
                    model
            , Cmd.none
            )



-- VIEW


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
    div [ class "flex-1" ]
        [ div [ class "color-light font-size-lg mb-md font-style-italic" ] [ text "Meals" ]
        , Keyed.node "div"
            []
            (meals
                |> Array.toList
                |> List.sortBy .name
                |> List.map viewKeyedMeals
            )
        ]


changeEditedMealName : Meal -> String -> Msg
changeEditedMealName meal value =
    ChangeEditedMeal (Just { meal | name = value })


changeEditedMealPrice : Meal -> String -> Msg
changeEditedMealPrice meal value =
    let
        price =
            Maybe.withDefault meal.price (String.toFloat value)
    in
    ChangeEditedMeal (Just { meal | price = price })


viewEditedMeal : Maybe Meal -> Html Msg
viewEditedMeal editedMeal =
    div [ class "flex-1 pl-md" ]
        [ case editedMeal of
            Just meal ->
                form [ class "color-light font-size-lg", onSubmit (UpdateMeal meal) ]
                    [ div [ class "mb-md" ]
                        [ span []
                            [ text "Editing " ]
                        , case meal.name of
                            "" ->
                                span [] [ text "meal" ]

                            name ->
                                b [] [ text ("\"" ++ name ++ "\"") ]
                        ]
                    , div [ class "mb-sm" ]
                        [ label [ class "font-size-md font-weight-700", for "name" ] [ text "Name" ]
                        , input
                            [ onInput (changeEditedMealName meal)
                            , value meal.name
                            , class "width-full sizing-border"
                            , name "name"
                            , required True
                            ]
                            []
                        ]
                    , div [ class "mb-lg" ]
                        [ label [ class "font-size-md font-weight-700", for "price" ] [ text "Price" ]
                        , input
                            [ onInput (changeEditedMealPrice meal)
                            , class "width-full sizing-border"
                            , name "price"
                            , type_ "number"
                            , step "0.01"
                            , Attr.min "0.1"
                            , value (String.fromFloat meal.price)
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

            Nothing ->
                div [ class "text-center color-light font-size-lg" ] [ text "No entry selected" ]
        ]


view : Model a -> Html Msg
view { meals, currentlyEditedMeal } =
    div [ class "bg-secondary height-full py-md px-md sizing-border" ]
        [ div [ class "mb-md flex justify-between" ]
            [ div [ class "font-size-xl font-weight-700 color-light" ]
                [ text "Bolec Gastro" ]
            , a
                [ class "button font-size-lg", href "/" ]
                [ text "Home" ]
            ]
        , div [ class "flex row" ]
            [ viewMeals meals
            , viewEditedMeal currentlyEditedMeal
            ]
        ]
