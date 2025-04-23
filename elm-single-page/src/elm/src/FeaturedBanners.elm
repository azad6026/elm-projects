module FeaturedBanners exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (alt, attribute, class, href, src)
import Http exposing (..)
import Json.Decode as Decode exposing (Decoder, field)
import Json.Decode.Pipeline exposing (optional, required)
import Platform.Cmd as Cmd



-- Main


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , subscriptions = \_ -> Sub.none
        , update = update
        , view = view
        }



--  Init


init : () -> ( Model, Cmd Msg )
init _ =
    ( Loading
    , httpRequest
    )



-- Model


type Model
    = Loading
    | Success Banners
    | Fail Http.Error


type Msg
    = GotFeaturedBanners (Result Http.Error Banners)


type alias Banners =
    { banners : List Banner }


type alias Banner =
    { id : String
    , href : String
    , target : String
    , imageUrl : String
    , imageSizes : String
    , srcset : String
    , title : String
    , linkText : String
    }



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotFeaturedBanners result ->
            case result of
                Ok banners ->
                    ( Success banners, Cmd.none )

                Err error ->
                    ( Fail error, Cmd.none )



-- View


view : Model -> Html Msg
view model =
    case model of
        Loading ->
            p [] [ text "Loading banners ..." ]

        Success data ->
            viewBanners data

        Fail error ->
            p [] [ text (errorMessage error) ]


errorMessage : Http.Error -> String
errorMessage error =
    case error of
        BadUrl url ->
            "The URL " ++ url ++ " was invalid"

        Timeout ->
            "Timeout, Unable to reach the server, try again"

        NetworkError ->
            "Unable to reach the server, try again"

        BadStatus 500 ->
            "Server error"

        BadStatus 400 ->
            "Bad request"

        BadStatus 404 ->
            "Not found"

        BadStatus _ ->
            "Bad status"

        BadBody body ->
            "Bad body: " ++ body


viewBanners : Banners -> Html Msg
viewBanners data =
    div [ class "featured-banners" ]
        (List.map
            viewBanner
            data.banners
        )


viewBanner : Banner -> Html Msg
viewBanner banner =
    figure [ class "featured-banner" ]
        [ img
            [ class "featured-banner-image"
            , src banner.imageUrl
            , attribute "srcset" banner.srcset
            , attribute "sizes" banner.imageSizes
            , attribute "loadding" "lazy"
            , alt banner.title
            ]
            []
        , figcaption [ class "featured-banner-text" ]
            [ h3 [ class "featured-banner-title" ]
                [ text banner.title ]
            , a
                [ class "featured-banner-link"
                , href banner.href
                ]
                [ text banner.linkText ]
            ]
        ]


httpRequest : Cmd Msg
httpRequest =
    Http.get
        { url = "/elm/featured-banners.json"
        , expect = Http.expectJson GotFeaturedBanners bannersDecoder
        }


bannersDecoder : Decoder Banners
bannersDecoder =
    Decode.map Banners
        (field "banners" (Decode.list bannerDecoder))


bannerDecoder : Decoder Banner
bannerDecoder =
    Decode.succeed Banner
        |> required "id" Decode.string
        |> required "href" Decode.string
        |> required "target" Decode.string
        |> required "imageUrl" Decode.string
        |> required "imageSizes" Decode.string
        |> required "srcset" Decode.string
        |> required "title" Decode.string
        |> optional "linkText" Decode.string "Read more"
