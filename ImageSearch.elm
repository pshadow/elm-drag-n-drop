module ImageSearch exposing (State, init, Msg, update, view)

import Html
import Html.App
import Html.Events
import Json.Decode
import Http
import Task
import Html.Attributes


type State
    = State
        { results : List PixabayImage
        }


init : State
init =
    State { results = [] }


type Msg
    = DoSearch
    | SearchFailure Http.Error
    | SearchSuccess PixabaySearchResponse
    | ImageSelected { url : String }


type alias Image =
    { url : String }


type alias PixabaySearchResponse =
    { totalHits : Int
    , hits : List PixabayImage
    }


pixabaySearchResponseDecoder : Json.Decode.Decoder PixabaySearchResponse
pixabaySearchResponseDecoder =
    Json.Decode.object2 PixabaySearchResponse
        (Json.Decode.at [ "totalHits" ] Json.Decode.int)
        (Json.Decode.at [ "hits" ] (Json.Decode.list pixabayImageDecoder))


type alias PixabayImage =
    { preview : String
    , webFormat : String
    }


pixabayImageDecoder : Json.Decode.Decoder PixabayImage
pixabayImageDecoder =
    Json.Decode.object2 PixabayImage
        (Json.Decode.at [ "previewURL" ] Json.Decode.string)
        (Json.Decode.at [ "webformatURL" ] Json.Decode.string)


update : Msg -> State -> ( State, Cmd Msg, Maybe Image )
update msg (State state) =
    case msg of
        DoSearch ->
            ( State state
            , Http.get pixabaySearchResponseDecoder "https://pixabay.com/api/?key=3203201-e7c4429a442f8f8d962b162c5&q=cute+cat&image_type=photo&pretty=true"
                |> Task.perform SearchFailure SearchSuccess
            , Nothing
            )

        SearchSuccess data ->
            ( State { state | results = data.hits }
            , Cmd.none
            , Nothing
            )

        SearchFailure _ ->
            ( State state, Cmd.none, Nothing )

        ImageSelected image ->
            ( State state, Cmd.none, Just image )


view : State -> Html.Html Msg
view (State state) =
    Html.div []
        [ Html.button
            [ Html.Events.onClick DoSearch
            ]
            [ Html.text "search" ]
        , Html.ul [] (List.map viewImage state.results)
        ]


viewImage : PixabayImage -> Html.Html Msg
viewImage image =
    Html.li []
        [ Html.img
            [ Html.Attributes.style
                [ ( "max-width", "100px" )
                , ( "max-height", "100px" )
                ]
            , Html.Attributes.src image.preview
            , Html.Events.onClick (ImageSelected { url = image.webFormat })
            ]
            []
        ]


main : Program Never
main =
    Html.App.program
        { init = ( init, Cmd.none )
        , subscriptions = \_ -> Sub.none
        , update =
            \msg oldModel ->
                let
                    ( newModel, cmd, selectedImage ) =
                        update msg oldModel

                    _ =
                        Debug.log "selectedImage" selectedImage
                in
                    ( newModel, cmd )
        , view = view
        }
