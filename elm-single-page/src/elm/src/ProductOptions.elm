module ProductOptions exposing (..)

import Array exposing (get)
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)



-- Main


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { options = sampleOptions
      , selectedOptionId = getInitialSelectedOptionId sampleOptions.options
      , selectedImageUrl = getSelectedOption sampleOptions.options |> .imageUrlOuter
      }
    , Cmd.none
    )



-- Model


type alias Model =
    { options : Options
    , selectedOptionId : String
    , selectedImageUrl : String
    }


type Msg
    = ChangedSelection String
    | ToggledImage String String


type alias Options =
    { options : List Option
    }


type alias Option =
    { id : String
    , color : String
    , imageUrlOuter : String
    , imageUrlInner : String
    , imageSizes : String
    , selected : Bool
    }



--  Sample Api


sampleOptions : Options
sampleOptions =
    { options =
        [ { id = "option-1"
          , color = "Colour 1"
          , imageUrlOuter = "https://picsum.photos/600/400?random=78"
          , imageUrlInner = "https://picsum.photos/600/400?random=79"
          , imageSizes = "(min-width: 768px) 50vw, 100vw"
          , selected = False
          }
        , { id = "option-2"
          , color = "Colour 2"
          , imageUrlOuter = "https://picsum.photos/600/400?random=7"
          , imageUrlInner = "https://picsum.photos/600/400?random=8"
          , imageSizes = "(min-width: 768px) 50vw, 100vw"
          , selected = True
          }
        , { id = "option-3"
          , color = "Colour 3"
          , imageUrlOuter = "https://picsum.photos/600/400?random=27"
          , imageUrlInner = "https://picsum.photos/600/400?random=28"
          , imageSizes = "(min-width: 768px) 50vw, 100vw"
          , selected = False
          }
        ]
    }



--  Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangedSelection id ->
            let
                selectedOption =
                    getSelectedItem id

                selectedOptions =
                    List.map
                        (\option ->
                            if option.id == id then
                                { option | selected = True }

                            else
                                { option | selected = False }
                        )
                        model.options.options

                updatedOptions =
                    { options = selectedOptions }
            in
            ( { model
                | selectedOptionId = selectedOption.id
                , options = updatedOptions
                , selectedImageUrl = selectedOption.imageUrlOuter
              }
            , Cmd.none
            )

        ToggledImage id imageUrl ->
            let
                selectedOption =
                    getSelectedItem id
            in
            if selectedOption.imageUrlOuter == imageUrl then
                ( { model | selectedImageUrl = selectedOption.imageUrlInner }, Cmd.none )

            else
                ( { model | selectedImageUrl = selectedOption.imageUrlOuter }, Cmd.none )



-- View


view : Model -> Html Msg
view model =
    let
        option =
            getSelectedOption model.options.options

        imageUrl =
            model.selectedImageUrl

        toggledText =
            if imageUrl == option.imageUrlOuter then
                "Toggle image"

            else
                "Close"
    in
    div []
        [ div [] (List.map (viewOption model.selectedOptionId) model.options.options)
        , figure [ class "option-image-figure" ]
            [ img [ class "option-image", src imageUrl, attribute "sizes" option.imageSizes, attribute "loading" "lazy", alt option.color ] []
            ]
        , button [ class "toggle-image", onClick (ToggledImage model.selectedOptionId imageUrl) ] [ text toggledText ]
        ]


viewOption : String -> Option -> Html Msg
viewOption selectedId option =
    let
        isChecked =
            selectedId == option.id
    in
    div []
        [ label [ class "option-label" ]
            [ text option.color
            , input
                [ type_ "radio"
                , name "option"
                , value option.id
                , checked isChecked
                , onInput ChangedSelection
                ]
                []
            ]
        ]



--  Helper functions


getInitialSelectedOptionId : List Option -> String
getInitialSelectedOptionId options =
    case List.filter (\option -> option.selected) options of
        [ option ] ->
            option.id

        _ ->
            "option-1"


getSelectedItem : String -> { id : String, imageUrlOuter : String, imageUrlInner : String, imageSizes : String, color : String }
getSelectedItem id =
    case List.filter (\option -> option.id == id) sampleOptions.options of
        [ option ] ->
            { id = option.id
            , color = option.color
            , imageSizes = option.imageSizes
            , imageUrlOuter = option.imageUrlOuter
            , imageUrlInner = option.imageUrlInner
            }

        _ ->
            { id = "Colour 1"
            , color = "Colour 1"
            , imageSizes = "(min-width: 768px) 50vw, 100vw"
            , imageUrlOuter = "https://picsum.photos/600/400?random=33"
            , imageUrlInner = "https://picsum.photos/600/400?random=34"
            }


getSelectedOption : List Option -> { imageUrlOuter : String, imageUrlInner : String, imageSizes : String, color : String }
getSelectedOption options =
    case List.filter (\option -> option.selected) options of
        [ option ] ->
            { color = option.color
            , imageSizes = option.imageSizes
            , imageUrlOuter = option.imageUrlOuter
            , imageUrlInner = option.imageUrlInner
            }

        _ ->
            { color = "Colour 1"
            , imageSizes = "(min-width: 768px) 50vw, 100vw"
            , imageUrlOuter = "https://picsum.photos/600/400?random=33"
            , imageUrlInner = "https://picsum.photos/600/400?random=34"
            }
