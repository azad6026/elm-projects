module Cards exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline exposing (optional, required)

import Card exposing (Card, Option)
import Card as CardModule -- ** Allow access to Card.Msg, Card.Model, etc. **
import Dict exposing (Dict)

-- MAIN

main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }

-- MODEL

type Model
    = Loading
    | CardsLoaded (Dict String CardModule.Model) -- ** Track each card's state **
    | Error Http.Error

-- ** Add wrapper message for card-specific messages **
type Msg
    = CardFetched (Result Http.Error (List Card))
    | CardMsg String CardModule.Msg

-- INIT

init : () -> (Model, Cmd Msg)
init _ =
    ( Loading, httpRequest )

-- UPDATE

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        CardFetched result ->
            case result of
                Ok cards ->
                    let
                        dict =
                            cards
                                |> List.map (\c -> (c.id, CardModule.init c)) -- ** Init each card **
                                |> Dict.fromList
                    in
                    ( CardsLoaded dict, Cmd.none )

                Err err ->
                    ( Error err, Cmd.none )

        CardMsg id cardMsg ->
            case model of
                CardsLoaded dict ->
                    case Dict.get id dict of
                        Just cardModel ->
                            let
                                (newCardModel, cardCmd) =
                                    CardModule.update cardMsg cardModel -- ** Forward update **
                            in
                            ( CardsLoaded (Dict.insert id newCardModel dict)
                            , Cmd.map (CardMsg id) cardCmd -- ** Wrap command **
                            )

                        Nothing ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

-- VIEW

view : Model -> Html Msg
view model =
    case model of
        Loading ->
            p [] [ text "Loading cards..." ]

        CardsLoaded dict ->
            viewCards dict

        Error err ->
            p [] [ text (errorMessage err) ]

viewCards : Dict String CardModule.Model -> Html Msg
viewCards dict =
    div [ class "cards" ]
        (dict
            |> Dict.toList
            |> List.map (\(id, cardModel) ->
                Html.map (CardMsg id) (CardModule.view cardModel) -- ** Route card views/messages **
            )
        )

-- HTTP

httpRequest : Cmd Msg
httpRequest =
    Http.get
        { url = "/elm/featured-banners.json"
        , expect = Http.expectJson CardFetched cardsDecoder
        }

-- DECODERS

cardsDecoder : Decoder (List Card)
cardsDecoder =
    Decode.field "cards" (Decode.list cardDecoder)

cardDecoder : Decoder Card
cardDecoder =
    Decode.succeed Card
        |> Pipeline.required "id" Decode.string
        |> Pipeline.required "title" Decode.string
        |> Pipeline.required "description" Decode.string
        |> Pipeline.required "options" (Decode.list optionDecoder)
        |> Pipeline.optional "buttonToggleState" Decode.bool False

optionDecoder : Decoder Option
optionDecoder =
    Decode.succeed Option
        |> Pipeline.required "id" Decode.string
        |> Pipeline.required "color" Decode.string
        |> Pipeline.required "imageUrlOuter" Decode.string
        |> Pipeline.required "imageUrlInner" Decode.string
        |> Pipeline.optionalAt [ "imageSizes" ] Decode.string "(min-width: 768px) 50vw, 100vw"
        |> Pipeline.required "price" Decode.int
        |> Pipeline.required "selected" Decode.bool

-- ERROR

errorMessage : Http.Error -> String
errorMessage error =
    case error of
        Http.BadUrl url -> "Bad URL: " ++ url
        Http.Timeout -> "Request timed out"
        Http.NetworkError -> "Network error"
        Http.BadStatus status -> "Status code: " ++ String.fromInt status
        Http.BadBody msg -> "Bad response: " ++ msg
