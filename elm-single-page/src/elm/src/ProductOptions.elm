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
    , imageUrlOuter : String
    , imageUrlInner : String
    , imageSizes : String
    , selected : Bool
    }


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
            getSelectedOption model.options.options
    in
    div []
        [ div [] (List.map (viewOption model.selectedOptionId) model.options.options)
        , div []
            [ img [ class "option-image", src option.imageUrlOuter, attribute "sizes" option.imageSizes, attribute "loading" "lazy", alt option.color ] []
            ]
        ]


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
