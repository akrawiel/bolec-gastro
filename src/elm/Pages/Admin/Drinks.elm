module Pages.Admin.Drinks exposing (Msg, update, viewAddOrEditDrink, viewDrinks)

import Array exposing (Array)
import Entities.Drink exposing (Drink, drinkDecoder)
import Entities.Meal exposing (Meal)
import Html exposing (Html, b, button, div, form, input, label, span, text)
import Html.Attributes as Attr exposing (class, for, name, required, step, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Html.Keyed as Keyed
import Html.Lazy exposing (lazy)
import Http
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


type alias EditedEntity =
    { currentlyEditedName : String
    , currentlyEditedPrice : String
    , currentlyEditedVolume : String
    }


type Msg
    = ChangeEditedDrink (Maybe Drink)
    | ChangeEditedEntity EditedEntity
    | UpdateDrink Drink
    | DrinkUpdated (Result Http.Error String)
    | SetAddingDrink
    | AddDrink NewDrink
    | DrinkAdded (Result Http.Error Drink)


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


addDrinkRequest : Model a -> NewDrink -> Cmd Msg
addDrinkRequest { apiUrl } newDrink =
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


update : Msg -> Model a -> ( Model a, Cmd Msg )
update msg model =
    case msg of
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

        SetAddingDrink ->
            ( { model
                | currentlyEditedDrink = Nothing
                , currentlyEditedMeal = Nothing
                , addingDrink = True
                , addingMeal = False
                , currentlyEditedName = ""
                , currentlyEditedPrice = ""
              }
            , Cmd.none
            )

        AddDrink addedDrink ->
            let
                newDrink =
                    case
                        ( model.currentlyEditedName
                        , model.currentlyEditedPrice |> String.toFloat
                        , model.currentlyEditedVolume |> String.toInt
                        )
                    of
                        ( name, Just price, Just volume ) ->
                            Just
                                { addedDrink
                                    | name = name
                                    , price = price
                                    , volume = volume
                                }

                        _ ->
                            Nothing
            in
            case newDrink of
                Just drink ->
                    ( model, addDrinkRequest model drink )

                Nothing ->
                    ( model, Cmd.none )

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



-- EVENTS


changeEditedName : EditedEntity -> String -> Msg
changeEditedName editedEntity value =
    ChangeEditedEntity { editedEntity | currentlyEditedName = value }


changeEditedPrice : EditedEntity -> String -> Msg
changeEditedPrice editedEntity value =
    ChangeEditedEntity { editedEntity | currentlyEditedPrice = value }


changeEditedVolume : EditedEntity -> String -> Msg
changeEditedVolume editedEntity value =
    ChangeEditedEntity { editedEntity | currentlyEditedVolume = value }



-- VIEW


viewDrinkForm : (DrinkExtendable b -> Msg) -> Model a -> DrinkExtendable b -> Html Msg
viewDrinkForm onDrinkSubmit { currentlyEditedName, currentlyEditedPrice, currentlyEditedVolume, addingDrink } drink =
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
                    [ text
                        (if addingDrink then
                            "Adding "

                         else
                            "Editing "
                        )
                    ]
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
                [ button [ class "button width-full", type_ "button", onClick (ChangeEditedDrink Nothing) ] [ text "Cancel" ]
                ]
            ]
        ]


viewAddOrEditDrink : Model a -> Html Msg
viewAddOrEditDrink model =
    case ( model.addingDrink, model.currentlyEditedDrink ) of
        ( True, _ ) ->
            viewDrinkForm AddDrink model { name = "", price = 0, volume = 0 }

        ( False, Just drink ) ->
            viewDrinkForm UpdateDrink model drink

        _ ->
            text ""


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
        , button [ class "button width-full font-size-lg", onClick SetAddingDrink ] [ text "+ Add drink" ]
        ]
