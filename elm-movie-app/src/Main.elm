port module Main exposing (main)

import Browser
import Html exposing (Html, text, div, ul, li, button, img)
import Html.Attributes exposing (style, class, src)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Debug
import Html exposing (figure)
import Html exposing (figcaption)


-- PORTS

port saveFavorites : List Int -> Cmd msg
port loadFavorites : (List Int -> msg) -> Sub msg


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


-- MODEL


type alias Movie =
    { id : Int
    , title : String
    , poster_path : String
    , overview : String
    }


type alias Model =
    { movies : List Movie
    , favorites : List Int
    , filter : String
    , isLoading : Bool
    , errorMessage : Maybe String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    -- We start with empty favorites, they will be loaded via subscription
    ( { movies = []
      , favorites = []
      , filter = "popular"
      , isLoading = True
      , errorMessage = Nothing
      }
    , getPopularMovies -- Keep fetching movies as before
    )


-- UPDATE


type Msg
    = GotMovies (Result Http.Error (List Movie))
    | ToggleFavorite Int
    | SetFilter String
    | LoadedFavorites (List Int) -- Message to receive favorites from local storage


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotMovies (Ok movies) ->
            ( { model | movies = movies, isLoading = False }, Cmd.none )

        GotMovies (Err error) ->
            ( { model | isLoading = False, errorMessage = Just (Debug.toString error) }, Cmd.none )

        ToggleFavorite id ->
            let
                isFavorite =
                    List.member id model.favorites

                updatedFavorites =
                    if isFavorite then
                        List.filter ((/=) id) model.favorites

                    else
                        id :: model.favorites
            in
            ( { model | favorites = updatedFavorites }
            , saveFavorites updatedFavorites -- Save updated favorites to local storage
            )

        SetFilter filter ->
            ( { model | filter = filter }, Cmd.none )

        LoadedFavorites loadedFavorites ->
            -- This message is received once on startup from the subscription
            ( { model | favorites = loadedFavorites }, Cmd.none )


-- VIEW


view : Model -> Html Msg
view model =
    div [ class "app-container" ]
        [ div [ class "filter-buttons" ]
              [ button [ class "filter-button", Html.Events.onClick (SetFilter "popular") ] [ text "Popular" ]
              , button [ class "filter-button", Html.Events.onClick (SetFilter "favorites") ] [ text "Favorites" ]
              ]
        , if model.isLoading then
            text "Loading..."

          else
            case model.errorMessage of
                Just message ->
                    text ("Error: " ++ message)

                Nothing ->
                    div [ style "display" "grid", style "grid-template-columns" "repeat(auto-fill, minmax(250px, 1fr))", style "gap" "10px" ]
                        (List.map (movieCard model.favorites) (getFilteredMovies model))
        ]


movieCard : List Int -> Movie -> Html Msg
movieCard favorites movie =
    let
        isFavorite =
            List.member movie.id favorites

        imageUrl =
            "https://image.tmdb.org/t/p/w200" ++ movie.poster_path
    in
    figure [ class "movie-card" ]
        [ img [ src imageUrl, style "width" "100%" ] []
        , figcaption [ style "padding" "10px" ]
              [ text movie.title
              , text ("(" ++ String.slice 0 4 movie.overview ++ ")")
              , button [ Html.Events.onClick (ToggleFavorite movie.id), class "favorite-button" ] [ text (if isFavorite then "Remove from Favorites" else "Add to Favorites") ]
              ]
        ]


getFilteredMovies : Model -> List Movie
getFilteredMovies model =
    case model.filter of
        "popular" ->
            model.movies

        "favorites" ->
            List.filter (\movie -> List.member movie.id model.favorites) model.movies

        _ ->
            model.movies


-- HTTP


getPopularMovies : Cmd Msg
getPopularMovies =
    let
        url =
            "https://api.themoviedb.org/3/movie/popular?api_key=fcd88911f1a3d29acb72510ad5c4413c&page=1"

        decoder =
            Decode.field "results" (Decode.list movieDecoder)
    in
    Http.get
        { url = url
        , expect = Http.expectJson GotMovies decoder
        }


movieDecoder : Decode.Decoder Movie
movieDecoder =
    Decode.map4 Movie
        (Decode.field "id" Decode.int)
        (Decode.field "title" Decode.string)
        (Decode.field "poster_path" Decode.string)
        (Decode.field "overview" Decode.string)


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    loadFavorites LoadedFavorites -- Subscribe to the incoming port
