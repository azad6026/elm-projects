port module Main exposing (main)

import Browser
import Html exposing (Html, Attribute, div, h1, ul, li, button, input, text, label, span, br)
import Html.Attributes exposing (class, placeholder, type_, value, disabled, id, style)
import Html.Events exposing (onClick, onInput, onSubmit)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Maybe exposing (withDefault)


-- PORTS (Communication with JavaScript / localStorage)

port saveExpenses : List Expense -> Cmd msg
port clearExpenses : () -> Cmd msg
port loadExpensesRequest : () -> Cmd msg
port loadExpensesReceiver : (Maybe (List Expense) -> msg) -> Sub msg


-- MODEL

type alias Model =
    { expenses : List Expense
    , descriptionInput : String
    , amountInput : String
    , nextId : Int -- Simple way to generate unique IDs
    , isLoading : Bool -- To prevent actions while loading initial data
    , descriptionError : Maybe String
    , amountError : Maybe String
    }


type alias Expense =
    { id : Int
    , description : String
    , amount : Float
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { expenses = []
      , descriptionInput = ""
      , amountInput = ""
      , nextId = 0
      , isLoading = True -- Start in loading state
      , descriptionError = Nothing
      , amountError = Nothing
      }
    , loadExpensesRequest () -- Request data from localStorage on init
    )


-- UPDATE

type Msg
    = UpdateDescription String
    | UpdateAmount String
    | AddExpense
    | RemoveExpense Int
    | ExpensesLoaded (Maybe (List Expense))
    | ClearAllExpenses
    | ValidateForm


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateDescription desc ->
            ( { model | descriptionInput = desc, descriptionError = Nothing, amountError = Nothing }, Cmd.none )

        UpdateAmount amountStr ->
            let
                filteredAmount =
                    String.filter
                        (\c ->
                            (c >= '0' && c <= '9') || c == '.'
                        )
                        amountStr
            in
            ( { model | amountInput = filteredAmount, descriptionError = Nothing, amountError = Nothing }, Cmd.none )

        AddExpense ->
            ( model, Cmd.none )

        ValidateForm ->
            let
                -- Validate the form
                descriptionValidation =
                    if String.isEmpty model.descriptionInput then
                        Just "Description cannot be empty."

                    else
                        Nothing

                amountValidation =
                    if String.isEmpty model.amountInput then
                        Just "Amount cannot be empty."

                    else
                        Nothing

                -- Attempt to convert amount string to Float
                maybeAmount =
                    String.toFloat model.amountInput

                newExpenseResult =
                    case maybeAmount of
                        Just amount ->
                            if not (String.isEmpty model.descriptionInput) && amount > 0 then
                                Just { id = model.nextId, description = model.descriptionInput, amount = amount }

                            else
                                Nothing

                        Nothing ->
                            Nothing
            in
            case ( descriptionValidation, amountValidation ) of
                ( Just descError, Just amountError ) ->
                    ( { model | descriptionError = Just descError, amountError = Just amountError }, Cmd.none )

                ( Just descError, Nothing ) ->
                    ( { model | descriptionError = Just descError, amountError = Nothing }, Cmd.none )

                ( Nothing, Just amountError ) ->
                    ( { model | descriptionError = Nothing, amountError = Nothing }, Cmd.none )

                ( Nothing, Nothing ) ->
                    case maybeAmount of
                        Just newAmount ->
                            if not (String.isEmpty model.descriptionInput) && newAmount > 0 then
                                let
                                    newExpense =
                                        { id = model.nextId
                                        , description = model.descriptionInput
                                        , amount = newAmount
                                        }

                                    updatedExpenses =
                                        newExpense :: model.expenses

                                    updatedModel =
                                        { model
                                            | expenses = updatedExpenses
                                            , descriptionInput = ""
                                            , amountInput = ""
                                            , nextId = model.nextId + 1
                                            , descriptionError = Nothing
                                            , amountError = Nothing
                                        }
                                in
                                ( updatedModel, saveExpenses updatedExpenses )

                            else
                                ( model, Cmd.none )

                        Nothing ->
                            ( model, Cmd.none )

        RemoveExpense idToRemove ->
            let
                updatedExpenses =
                    List.filter (\expense -> expense.id /= idToRemove) model.expenses
            in
            ( { model | expenses = updatedExpenses }, saveExpenses updatedExpenses )

        ExpensesLoaded maybeExpenses ->
            case maybeExpenses of
                Just loadedExpenses ->
                    let
                        maxId =
                            List.map .id loadedExpenses
                                |> List.maximum
                                |> Maybe.withDefault -1
                    in
                    ( { model
                        | expenses = loadedExpenses
                        , nextId = maxId + 1
                        , isLoading = False
                        , descriptionError = Nothing
                        , amountError = Nothing
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( { model
                        | expenses = []
                        , nextId = 0
                        , isLoading = False
                        , descriptionError = Nothing
                        , amountError = Nothing
                      }
                    , Cmd.none
                    )

        ClearAllExpenses ->
            ( { model
                | expenses = []
                , descriptionInput = ""
                , amountInput = ""
                , nextId = 0
                , descriptionError = Nothing
                , amountError = Nothing
              }
            , clearExpenses ()
            )


-- VIEW

view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ h1 [] [ text "Expense Tracker" ]
        , viewInputForm model
        , viewExpenseList model.expenses
        , viewTotal model.expenses
        , viewClearButton model
        ]


viewInputForm : Model -> Html Msg
viewInputForm model =
    -- Using div instead of form and preventing default submit behavior
    -- This avoids page reloads if enter is pressed in an input field
    div [ class "form-group" ]
        [ label [ Html.Attributes.for "description" ] [ text "Description" ]
        , div []
            [input
                [ id "description"
                , type_ "text"
                , placeholder "Enter expense description"
                , value model.descriptionInput
                , onInput UpdateDescription
                , disabled model.isLoading -- Disable while loading
                ]
            ]
        , br [] []
        , div [ class "error-message"
                   , style "color" "red"
                   ]
            [ Maybe.withDefault "" model.descriptionError
                |> text
            ]
        , label [ Html.Attributes.for "amount" ] [ text "Amount" ]
        , br [] []
        , div []
            [ input
                [ id "amount"
                , type_ "number"
                , placeholder "Enter amount"
                , value model.amountInput
                , onInput UpdateAmount
                , disabled model.isLoading -- Disable while loading
                ]
            ]
        , br [] []
        , div [ class "error-message"
                   , style "color" "red"
                   ]
            [ Maybe.withDefault "" model.amountError
                |> text
            ]
        , button
            [ onClick ValidateForm
            , disabled model.isLoading
            ]
            [ text "Add Expense" ]
        ]


viewTotal : List Expense -> Html Msg
viewTotal expenses =
    let
        total =
            List.map .amount expenses |> List.sum
    in
    div [ class "total" ] [ text ("Total Expenses: $" ++ String.fromFloat total) ]


viewClearButton : Model -> Html Msg
viewClearButton model =
    button
        [ class "clear-button"
        , onClick ClearAllExpenses
        , disabled (model.isLoading || List.isEmpty model.expenses)
        ]
        [ text "Clear All Expenses" ]


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    loadExpensesReceiver ExpensesLoaded

viewExpenseList : List Expense -> Html Msg
viewExpenseList expenses =
    ul [] (List.map viewExpense expenses)

viewExpense : Expense -> Html Msg
viewExpense expense =
    li [ class "expense-item" ]
        [ span [] [ text (expense.description ++ ": $" ++ String.fromFloat expense.amount) ]
        , button [ onClick (RemoveExpense expense.id) ] [ text "Remove" ]
        ]

-- JSON Encoding/Decoding

encodeExpense : Expense -> Encode.Value
encodeExpense expense =
    Encode.object
        [ ( "id", Encode.int expense.id )
        , ( "description", Encode.string expense.description )
        , ( "amount", Encode.float expense.amount )
        ]


decodeExpense : Decoder Expense
decodeExpense =
    Decode.map3 Expense
        (Decode.field "id" Decode.int)
        (Decode.field "description" Decode.string)
        (Decode.field "amount" Decode.float)


encodeExpensesList : List Expense -> Encode.Value
encodeExpensesList expenses =
    Encode.list encodeExpense expenses


decodeExpensesList : Decoder (List Expense)
decodeExpensesList =
    Decode.list decodeExpense


-- MAIN

main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
