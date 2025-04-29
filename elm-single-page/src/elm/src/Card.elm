module Card exposing (Model, Msg(..), init, update, view, Card, Option)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)


-- TYPES

type alias Card =
    { id : String
    , title : String
    , description : String
    , options : List Option
    , buttonToggleState : Bool
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

-- ** Local model per card **
type alias Model =
    Card

-- ** Local message type for a card **
type Msg
    = ToggledImage

-- ** Initialize a card into a card-specific model **
init : Card -> Model
init card =
    card

-- ** Handle ToggledImage message locally **
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        ToggledImage ->
            ( { model | buttonToggleState = not model.buttonToggleState }, Cmd.none )

-- ** View now uses Card.Model and returns Html Msg **
view : Model -> Html Msg
view card =
    let
    
        selectedRadio =
            List.filter (\option -> option.selected) card.options
                |> List.head

        viewImage =
            case selectedRadio of
                Just option ->
                    if card.buttonToggleState then
                        { imageUrl = option.imageUrlInner, id = option.id }
                    else
                        { imageUrl = option.imageUrlOuter, id = option.id }

                Nothing ->
                    { imageUrl = "", id = "" }
    in
    article [ class "card" ]
        [ button
            [ class "toggle-image"
            , onClick ToggledImage -- ** Local Msg, not passed in anymore **
            ]
            [ text "Toggle image" ]
        , figure [ class "figure" ]
            [ img [ class "image", src viewImage.imageUrl, alt card.title ] []
            , figcaption [ class "text" ]
                [ h2 [ class "title" ] [ text card.title ]
                , p [ class "description" ] [ text card.description ]
                , div [ class "options" ]
                    (List.map (viewOption viewImage.id) card.options)
                ]
            ]
        ]


viewOption : String -> Option -> Html Msg
viewOption selectedOption option =
    label [ class "option-label", for option.id ]
        [ input
            [ class "option"
            , type_ "radio"
            , id option.id
            , name "option"
            , value option.id
            , checked (option.id == selectedOption)
            ]
            []
        ]
