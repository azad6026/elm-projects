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



-- type ChangedRadio
--     = Option
-- Update


type Msg
    = GotItems (Result Http.Error Items)
    | ChangedOption String NewOption
    | ToggledImage String ItemImage


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

        ToggledImage cardId imageUrl ->
            case model of
                Success items ->
                    let
                        updatedCardWithToggledImage card =
                            if card.id == cardId then
                                { card
                                    | toggledImage = imageUrl
                                }

                            else
                                card
                    in
                    ( Success { items | items = List.map updatedCardWithToggledImage items.items }, Cmd.none )

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
    div [ class "items" ] (List.map viewItem data.items)


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

        --     justSelected =
        --         List.filter (\i -> String.toLower i.selectedOption == i.color) item.options
        --             |> List.head
        -- selectedOption =
        --     List.filter (\i -> i.selected) item.options
        --         |> List.head
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
    in
    article [ class "item" ]
        [ button [ class "toggle-image" ]
            [ text " Toggle image" ]
        , figure [ class "figure" ]
            [ img [ class "image", src optionsInfo.imageUrlOuter ]
                []
            , figcaption
                [ class "text" ]
                [ h2 [ class "title" ] [ text item.title ]
                , p [ class "description" ] [ text item.description ]
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
