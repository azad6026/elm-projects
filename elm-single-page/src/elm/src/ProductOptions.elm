module ProductOptions exposing (..)

import Array exposing (get)
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)



-- Main


init : () -> ( Model, Cmd Msg )
init _ =
    ( { options = sampleOptions
      , selectedOptionId = getInitialSelectedOptionId sampleOptions.options
      }
    , Cmd.none
    )


getInitialSelectedOptionId : List Option -> String
getInitialSelectedOptionId options =
    case List.filter (\option -> option.selected) options of
        [ option ] ->
            option.id

        _ ->
            "option-1"


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }



-- Model


type alias Model =
    { options : Options
    , selectedOptionId : String
    }


type Msg
    = ChangedSelection String


type alias Options =
    { options : List Option }


type alias Option =
    { id : String
    , color : String
    , imageUrl : String
    , imageSizes : String
    , selected : Bool
    }


sampleOptions : Options
sampleOptions =
    { options =
        [ { id = "option-1"
          , color = "Colour 1"
          , imageUrl = "https://bellroy-cms-images.imgix.net/2024-Tech-collection-homepage-subbanner-1.jpg"
          , imageSizes = "(min-width: 768px) 50vw, 100vw"
          , selected = False
          }
        , { id = "option-2"
          , color = "Colour 2"
          , imageUrl = "https://bellroy-cms-images.imgix.net/2024-laneway-homepage-subbanner-2-v2.jpg"
          , imageSizes = "(min-width: 768px) 50vw, 100vw"
          , selected = True
          }
        , { id = "option-3"
          , color = "Colour 3"
          , imageUrl = "https://bellroy-cms-images.imgix.net/2024-Bundles-homepage-subbanner-position-3.jpg"
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
                | selectedOptionId = id
                , options = updatedOptions
              }
            , Cmd.none
            )



-- View


view : Model -> Html Msg
view model =
    let
        option =
            getSelectedImage model.options.options
    in
    div []
        [ div [] (List.map (viewOption model.selectedOptionId) model.options.options)
        , div []
            [ img [ class "option-image", src option.imageUrl, attribute "sizes" option.imageSizes, attribute "loading" "lazy", alt option.color ] []
            ]
        ]


getSelectedImage : List Option -> { imageUrl : String, imageSizes : String, color : String }
getSelectedImage options =
    case List.filter (\option -> option.selected) options of
        [ option ] ->
            { color = option.color
            , imageSizes = option.imageSizes
            , imageUrl = option.imageUrl
            }

        _ ->
            { color = "Colour 1"
            , imageSizes = "(min-width: 768px) 50vw, 100vw"
            , imageUrl = "https://bellroy-cms-images.imgix.net/2024-Tech-collection-homepage-subbanner-1.jpg"
            }


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
