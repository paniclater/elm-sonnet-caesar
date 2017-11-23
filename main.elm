module Main exposing (..)

import Char
import Html exposing (..)
import Html.Events exposing (onClick)
import Http exposing (Request, get)
import Json.Decode exposing (Decoder, decodeString, list, string)
import Json.Decode.Pipeline exposing (decode, required)
import Random


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
    , selectedIndex : Int
    }


initModel : Model
initModel =
    Model [] "" 0


init : ( Model, Cmd Msg )
init =
    ( initModel
    , getPoemsCmd
    )



-- UPDATE


type Msg
    = GetPoems
    | GotPoems (List Poem)
    | GetRandomIndex
    | SetSelectedIndex Int
    | ShowError String


getPoemsCmd : Cmd Msg
getPoemsCmd =
    Http.send
        (\res ->
            case res of
                Ok poems ->
                    GotPoems poems

                Err httpErr ->
                    ShowError (toString httpErr)
        )
        poemRequest


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetPoems ->
            model
                ! [ getPoemsCmd ]

        GetRandomIndex ->
            model ! [ Random.generate SetSelectedIndex (Random.int 0 (List.length model.poems - 1)) ]

        GotPoems poems ->
            { model | poems = poems }
                ! [ Random.generate SetSelectedIndex (Random.int 0 (List.length poems - 1)) ]

        ShowError error ->
            { model | error = error } ! []

        SetSelectedIndex int ->
            { model | selectedIndex = int } ! []


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


caesarCipher : Int -> String -> String
caesarCipher offset input =
    String.foldr (\a b -> String.cons (caeserCipherChar a offset) b) "" input


caeserCipherChar : Char -> Int -> Char
caeserCipherChar input offset =
    let
        code =
            Char.toCode input

        newCode =
            code + offset
    in
        if newCode > 122 then
            Char.fromCode (97 + newCode - 122)
        else
            Char.fromCode newCode



-- toCode converts char to int
-- A is 65
-- Z is 90
-- a is
-- |> required "author" string
-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


lines : (String -> String) -> List String -> List (Html Msg)
lines cipher l =
    List.foldl (\a b -> b ++ [ p [] [ text (cipher a) ] ]) [] l


showPoem : Poem -> List (Html Msg)
showPoem poem =
    let
        cipher =
            caesarCipher 1
    in
        [ h2 [] [ text (((++) "Title: " poem.title)) ], h3 [] [ text (((++) "Author: " poem.author)) ], h4 [] (lines (\a -> a) poem.lines) ]


showPoems : List Poem -> Int -> List (Html Msg)
showPoems poems index =
    let
        ps =
            List.head (List.drop index poems)
    in
        case ps of
            Nothing ->
                [ h2 [] [ text "Click to load a poem" ] ]

            Just poem ->
                showPoem poem


view : Model -> Html Msg
view model =
    let
        poems =
            showPoems model.poems model.selectedIndex

        getPoemsButton =
            ([ button [ onClick GetPoems ] [ text "Load Poems" ] ])

        getRandomPoemButton =
            ([ button [ onClick GetRandomIndex ] [ text "Get Random Poem" ] ])
    in
        div [] (poems ++ getRandomPoemButton)
