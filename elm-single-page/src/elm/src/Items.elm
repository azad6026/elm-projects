module Items exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode exposing (Decoder, bool, field, int, list, map, string, succeed)
import Json.Decode.Pipeline as Pipeline exposing (optional, required)
import String



-- Main


init : () -> ( Model, Cmd Msg )
init _ =
    ( Loading, httpRequest )


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }



-- Model


type Model
    = Loading
    | Success Items
    | Failed Http.Error


type alias Items =
    { items : List Item
    }


type alias Item =
    { id : String
    , itemType : String
    , initialPrice : Int
    , title : String
    , description : String
    , tags : String
    , options : List Option
    , selectedOption : NewOption
    , toggledImage : ItemImage
    }


type alias Option =
    { id : String
    , color : String
    , imageUrlOuter : String
    , imageUrlInner : String
    , imageSizes : String
    , price : Int
    , selected : Bool
    }


type NewOption
    = Yellow
    | Red
    | Royalblue
    | Green


type ItemImage
    = Outer
    | Inner


type SortOption
    = PriceLowToHigh
    | PriceHighToLow


type ItemCategory
    = All
    | Type1
    | Type2
    | Type3
    | Type4



-- Update


type Msg
    = GotItems (Result Http.Error Items)
    | ChangedOption String NewOption
    | ToggledImage String ItemImage
    | SortedItems SortOption
    | FilteredTypes ItemCategory


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotItems result ->
            case result of
                Ok items ->
                    ( Success items, Cmd.none )

                Err error ->
                    ( Failed error, Cmd.none )

        ChangedOption cardId newOption ->
            case model of
                Success items ->
                    let
                        updateCardWithNewSelectedOption card =
                            if card.id == cardId then
                                { card | selectedOption = newOption }

                            else
                                card
                    in
                    ( Success { items | items = List.map updateCardWithNewSelectedOption items.items }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ToggledImage cardId toggledImage ->
            case model of
                Success items ->
                    let
                        newImage =
                            case toggledImage of
                                Outer ->
                                    Inner

                                Inner ->
                                    Outer

                        updatedCardWithToggledImage card =
                            if card.id == cardId then
                                { card
                                    | toggledImage = newImage
                                }

                            else
                                card
                    in
                    ( Success { items | items = List.map updatedCardWithToggledImage items.items }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        SortedItems selectedSortOption ->
            case model of
                Success items ->
                    case selectedSortOption of
                        PriceLowToHigh ->
                            ( Success { items | items = List.sortBy .initialPrice items.items }, Cmd.none )

                        PriceHighToLow ->
                            ( Success { items | items = List.sortBy .initialPrice items.items |> List.reverse }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        FilteredTypes itemType ->
            case model of
                Success items ->
                    ( Success { items | items = List.filter (\i -> i.itemType == convertToString itemType) items.items }, Cmd.none )

                _ ->
                    ( model, Cmd.none )



-- View


view : Model -> Html Msg
view model =
    case model of
        Loading ->
            p [] [ text "Loading items ..." ]

        Success data ->
            viewItems data

        Failed _ ->
            p [] [ text "We are not able to load items now ..." ]


viewItems : Items -> Html Msg
viewItems data =
    section [ class "page" ]
        [ h1 [ class "main-title" ] [ text "List of items" ]
        , select [ class "select", onInput frmStringToSortedItems ]
            [ option [ class "select-option", value "PriceLowToHigh" ]
                [ text "Price  -> Low To High" ]
            , option [ class "select-option", value "PriceHighToLow" ]
                [ text "Price  -> High To Low" ]
            ]
        , select [ class "select", onInput frmStringToItemType ]
            [ option [ class "select-option", value "all" ]
                [ text "All" ]
            , option [ class "select-option", value "type1" ]
                [ text "Type 1" ]
            , option [ class "select-option", value "type2" ]
                [ text "Type 2" ]
            , option [ class "select-option", value "type3" ]
                [ text "Type 3" ]
            , option [ class "select-option", value "type4" ]
                [ text "Type 4" ]
            ]
        , div
            [ class "items" ]
            (List.map viewItem data.items)
        ]


viewItem : Item -> Html Msg
viewItem item =
    let
        selecetdItem =
            case item.selectedOption of
                Yellow ->
                    List.filter (\op -> op.color == "yellow") item.options

                Red ->
                    List.filter (\op -> op.color == "red") item.options

                Royalblue ->
                    List.filter (\op -> op.color == "royalblue") item.options

                Green ->
                    List.filter (\op -> op.color == "green") item.options

        optionsInfo =
            case List.head selecetdItem of
                Just option ->
                    { id = option.id
                    , imageUrlOuter = option.imageUrlOuter
                    , imageUrlInner = option.imageUrlInner
                    , price = option.price
                    , selected = True
                    }

                Nothing ->
                    { id = ""
                    , imageUrlOuter = ""
                    , imageUrlInner = ""
                    , price = 0
                    , selected = False
                    }

        imgeUrl =
            case item.toggledImage of
                Outer ->
                    optionsInfo.imageUrlOuter

                Inner ->
                    optionsInfo.imageUrlInner
    in
    article [ class "item" ]
        [ button [ class "toggle-image", onClick (ToggledImage item.id item.toggledImage) ]
            [ text " Toggle image" ]
        , figure [ class "figure" ]
            [ img [ class "image", src imgeUrl ]
                []
            , figcaption
                [ class "text" ]
                [ strong [ class "type" ] [ text item.itemType ]
                , h2 [ class "title" ]
                    [ text item.title
                    ]
                , p [ class "description" ] [ text item.description ]
                , p [ class "price" ] [ text (String.fromInt item.initialPrice) ]
                , fieldset [ class "options" ]
                    (List.map (viewOption item) item.options)
                ]
            ]
        ]


viewOption : Item -> Option -> Html Msg
viewOption item option =
    -- let
    --     isChecked =
    --         if option.id == selectedOption then
    --             True
    --         else
    --             False
    -- in
    label [ class "label", for ("group" ++ item.id) ]
        [ input
            [ type_ "radio"
            , class "option"
            , value option.id
            , id ("group" ++ item.id)
            , name ("options-" ++ item.id)
            , checked (item.selectedOption == stringToNewOption option.color)
            , onClick (ChangedOption item.id (stringToNewOption option.color))
            ]
            []
        ]



-- Http


httpRequest : Cmd Msg
httpRequest =
    Http.get
        { url = "/elm/items.json"
        , expect = Http.expectJson GotItems itemsDecoder
        }



-- Decoders


itemsDecoder : Decoder Items
itemsDecoder =
    Decode.map Items
        (field "items" (Decode.list itemDecoder))


itemDecoder : Decoder Item
itemDecoder =
    Decode.succeed Item
        |> Pipeline.required "id" string
        |> Pipeline.required "itemType" string
        |> Pipeline.required "initialPrice" int
        |> Pipeline.required "title" string
        |> Pipeline.required "description" string
        |> Pipeline.optional "tags " string ""
        |> Pipeline.required "options" (Decode.list optionDecoder)
        |> Pipeline.hardcoded Yellow
        |> Pipeline.hardcoded Outer


optionDecoder : Decoder Option
optionDecoder =
    Decode.succeed Option
        |> Pipeline.required "id" string
        |> Pipeline.required "color" string
        |> Pipeline.required "imageUrlOuter" string
        |> Pipeline.required "imageUrlInner" string
        |> Pipeline.required "imageSizes" string
        |> Pipeline.required "price" int
        |> Pipeline.optional "selected" bool False



-- Helper functions


stringToNewOption : String -> NewOption
stringToNewOption color =
    case String.toLower color of
        "yellow" ->
            Yellow

        "red" ->
            Red

        "royalblue" ->
            Royalblue

        "green" ->
            Green

        _ ->
            Yellow


frmStringToItemType : String -> Msg
frmStringToItemType selectedItemType =
    case selectedItemType of
        "type1" ->
            FilteredTypes Type1

        "type2" ->
            FilteredTypes Type2

        "type3" ->
            FilteredTypes Type3

        "type4" ->
            FilteredTypes Type4

        _ ->
            FilteredTypes All


frmStringToSortedItems : String -> Msg
frmStringToSortedItems selectOptinValue =
    case selectOptinValue of
        "PriceLowToHigh" ->
            SortedItems PriceLowToHigh

        "PriceHighToLow" ->
            SortedItems PriceHighToLow

        _ ->
            SortedItems PriceLowToHigh


convertToString : ItemCategory -> String
convertToString filterType =
    case filterType of
        Type1 ->
            "type1"

        Type2 ->
            "type2"

        Type3 ->
            "type3"

        Type4 ->
            "type4"

        All ->
            "all"
