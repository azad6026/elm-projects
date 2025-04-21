module Carousel exposing (main)

import Browser
import Html exposing (Html, button, div, img, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import List.Extra exposing (getAt)



-- MODEL


type alias Model =
    { images : List String
    , currentIndex : Int
    }


init : Model
init =
    { images =
        [ "https://picsum.photos/id/237/600/400"
        , "https://picsum.photos/id/238/600/400"
        , "https://picsum.photos/id/239/600/400"
        ]
    , currentIndex = 0
    }



-- UPDATE


type Msg
    = Next
    | Prev


update : Msg -> Model -> Model
update msg model =
    let
        lastIndex =
            List.length model.images - 1
    in
    case msg of
        Next ->
            { model | currentIndex = remainderBy (lastIndex + 1) (model.currentIndex + 1) }

        Prev ->
            { model
                | currentIndex =
                    if model.currentIndex == 0 then
                        lastIndex

                    else
                        model.currentIndex - 1
            }



-- VIEW


view : Model -> Html Msg
view model =
    let
        currentImage =
            List.Extra.getAt model.currentIndex model.images
                |> Maybe.withDefault ""
    in
    div [ style "text-align" "center" ]
        [ img [ src currentImage, style "width" "600px", style "height" "400px" ] []
        , div []
            [ button [ onClick Prev, style "margin" "10px" ] [ text "Prev" ]
            , button [ onClick Next ] [ text "Next" ]
            ]
        ]



-- MAIN


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }
