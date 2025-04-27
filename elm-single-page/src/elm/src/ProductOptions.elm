module ProductOptions exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List exposing (product)



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
    ( { product = sampleProduct
      , selectedOptionId = getInitialSelectedOptionId sampleProduct.options
      , selectedImageUrl = getSelectedOption sampleProduct.options |> .imageUrlOuter
      }
    , Cmd.none
    )



-- Model


type alias Model =
    { product : Product
    , selectedOptionId : String
    , selectedImageUrl : String
    }


type Msg
    = ChangedSelection String
    | ToggledImage String String


type alias Product =
    { title : String
    , description : String
    , options : List Option
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



--  Sample Api


sampleProduct : Product
sampleProduct =
    { title = "Product Options"
    , description = "Choose your colour"
    , options =
        [ { id = "option-1"
          , color = "Colour 1"
          , imageUrlOuter = "https://picsum.photos/600/400?random=78"
          , imageUrlInner = "https://picsum.photos/600/400?random=79"
          , imageSizes = "(min-width: 768px) 50vw, 100vw"
          , price = 100
          , selected = False
          }
        , { id = "option-2"
          , color = "Colour 2"
          , imageUrlOuter = "https://picsum.photos/600/400?random=7"
          , imageUrlInner = "https://picsum.photos/600/400?random=8"
          , imageSizes = "(min-width: 768px) 50vw, 100vw"
          , price = 200
          , selected = True
          }
        , { id = "option-3"
          , color = "Colour 3"
          , imageUrlOuter = "https://picsum.photos/600/400?random=27"
          , imageUrlInner = "https://picsum.photos/600/400?random=28"
          , imageSizes = "(min-width: 768px) 50vw, 100vw"
          , price = 300
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
                    getSelectedItem id model.product.options

                selectedOptions =
                    List.map
                        (\option ->
                            if option.id == id then
                                { option | selected = True }

                            else
                                { option | selected = False }
                        )
                        model.product.options

                updatedOptions =
                    { title = model.product.title
                    , description = model.product.description
                    , options = selectedOptions
                    }
            in
            ( { model
                | selectedOptionId = selectedOption.id
                , product = updatedOptions
                , selectedImageUrl = selectedOption.imageUrlOuter
              }
            , Cmd.none
            )

        ToggledImage id imageUrl ->
            let
                selectedOption =
                    getSelectedItem id model.product.options
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
            getSelectedOption model.product.options

        imageUrl =
            model.selectedImageUrl

        toggledText =
            if imageUrl == option.imageUrlOuter then
                "Toggle image"

            else
                "Close"
    in
    div [ class "product-wrapper" ]
        [ figure [ class "product-figure" ]
            [ img [ class "product-image", src imageUrl, attribute "sizes" option.imageSizes, attribute "loading" "lazy", alt option.color ]
                []
            , figcaption
                [ class "product-text" ]
                [ p [ class "product-price" ] [ text (String.fromInt option.price ++ " $") ]
                , h3 [ class "product-title" ] [ text model.product.title ]
                , p [ class "product-description" ] [ text model.product.description ]
                ]
            , button [ class "product-toggle-image", onClick (ToggledImage model.selectedOptionId imageUrl) ] [ text toggledText ]
            ]
        , div [ class "product-options" ] (List.map (viewOption model.selectedOptionId) model.product.options)
        ]


viewOption : String -> Option -> Html Msg
viewOption selectedId option =
    let
        isChecked =
            selectedId == option.id
    in
    label [ class "option-label" ]
        [ text option.color
        , input
            [ type_ "radio"
            , name "option"
            , value option.id
            , checked isChecked
            , onInput ChangedSelection
            , class "option-radio"
            ]
            []
        ]



--  Helper functions


getInitialSelectedOptionId : List Option -> String
getInitialSelectedOptionId options =
    case List.filter (\option -> option.selected) options of
        [ option ] ->
            option.id

        _ ->
            "option-1"


getSelectedItem : String -> List Option -> { id : String, imageUrlOuter : String, imageUrlInner : String, imageSizes : String, color : String }
getSelectedItem id product =
    case List.filter (\option -> option.id == id) product of
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


getSelectedOption : List Option -> { imageUrlOuter : String, imageUrlInner : String, imageSizes : String, color : String, price : Int }
getSelectedOption options =
    case List.filter (\option -> option.selected) options of
        [ option ] ->
            { color = option.color
            , imageSizes = option.imageSizes
            , imageUrlOuter = option.imageUrlOuter
            , imageUrlInner = option.imageUrlInner
            , price = option.price
            }

        _ ->
            { color = "Colour 1"
            , imageSizes = "(min-width: 768px) 50vw, 100vw"
            , imageUrlOuter = "https://picsum.photos/600/400?random=33"
            , imageUrlInner = "https://picsum.photos/600/400?random=34"
            , price = 100
            }
