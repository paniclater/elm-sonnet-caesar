module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (onClick)
import Http exposing (Request, get)
import Json.Decode exposing (Decoder, decodeString, list, string)
import Json.Decode.Pipeline exposing (decode, required)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { poems : List Poem
    , error : String
    }


initModel : Model
initModel =
    Model [] ""


init : ( Model, Cmd Msg )
init =
    ( initModel
    , Cmd.none
    )



-- UPDATE


type Msg
    = GetPoems
    | GotPoems (List Poem)
    | ShowError String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetPoems ->
            model
                ! [ Http.send
                        (\res ->
                            case res of
                                Ok poems ->
                                    GotPoems poems

                                Err httpErr ->
                                    ShowError (toString httpErr)
                        )
                        poemRequest
                  ]

        GotPoems poems ->
            { model | poems = poems } ! []

        ShowError error ->
            { model | error = error } ! []


type alias Poem =
    { title : String
    , author : String
    , lines : List String
    }



-- HTTP


poemRequest : Request (List Poem)
poemRequest =
    Http.get "http://localhost:4000/api/poems" (list poemDecoder)


poemDecoder : Decoder Poem
poemDecoder =
    decode Poem
        |> required "title" string
        |> required "author" string
        |> required "lines" (list string)



-- |> required "author" string
-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


lines : List String -> List (Html Msg)
lines l =
    List.foldl (\a b -> b ++ [ p [] [ text a ] ]) [] l


showPoem : Poem -> List (Html Msg)
showPoem poem =
    [ h2 [] [ text <| (++) "Title: " poem.title ], h3 [] [ text <| (++) "Author: " poem.author ], h4 [] (lines poem.lines) ]


showPoems poems =
    case poems of
        [] ->
            [ h2 [] [ text "Click to load a poem" ] ]

        _ ->
            List.foldl (\a b -> b ++ (showPoem a)) [] poems


view : Model -> Html Msg
view model =
    div [] ((showPoems model.poems) ++ ([ button [ onClick GetPoems ] [ text "Load Poems" ] ]))



-- [ poem decodedPoem ]            ]
