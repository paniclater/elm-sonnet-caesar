module Main exposing (..)

import Char
import Html exposing (..)
import Html.Attributes exposing (class, disabled)
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
    , isEncrypted : Bool
    , selectedIndex : Int
    }


initModel : Model
initModel =
    Model [] "" False 0


init : ( Model, Cmd Msg )
init =
    ( initModel
    , getPoemsCmd
    )



-- UPDATE


type Msg
    = DecryptPeom (Maybe Poem)
    | EncryptPeom (Maybe Poem)
    | GetPoems
    | GotPoems (List Poem)
    | GetRandomIndex (Maybe Poem)
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
        DecryptPeom maybePoem ->
            case maybePoem of
                Nothing ->
                    model ! []

                Just poem ->
                    { model
                        | isEncrypted = False
                        , poems =
                            (List.map
                                (\p ->
                                    if p.title == poem.title then
                                        { p | title = (caesarCipher False p.title), author = (caesarCipher False p.author), lines = List.map (caesarCipher False) p.lines }
                                    else
                                        p
                                )
                                model.poems
                            )
                    }
                        ! []

        EncryptPeom maybePoem ->
            case maybePoem of
                Nothing ->
                    model ! []

                Just poem ->
                    { model
                        | isEncrypted = True
                        , poems =
                            (List.map
                                (\p ->
                                    if p.title == poem.title then
                                        { p | title = (caesarCipher True p.title), author = (caesarCipher True p.author), lines = List.map (caesarCipher True) p.lines }
                                    else
                                        p
                                )
                                model.poems
                            )
                    }
                        ! []

        GetPoems ->
            model
                ! [ getPoemsCmd ]

        GetRandomIndex maybePoem ->
            let
                ps =
                    case maybePoem of
                        Nothing ->
                            model.poems

                        Just poem ->
                            if model.isEncrypted then
                                (List.map
                                    (\p ->
                                        if p.title == poem.title then
                                            { p | title = (caesarCipher False p.title), author = (caesarCipher False p.author), lines = List.map (caesarCipher False) p.lines }
                                        else
                                            p
                                    )
                                    model.poems
                                )
                            else
                                model.poems
            in
                { model | poems = ps, isEncrypted = False } ! [ Random.generate SetSelectedIndex (Random.int 0 (List.length model.poems - 1)) ]

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


cipherFolder : Bool -> Char -> (String -> String)
cipherFolder encrypt a b =
    let
        offset =
            case encrypt of
                True ->
                    1

                False ->
                    -1
    in
        String.cons (caeserCipherChar a offset) b


caesarCipher : Bool -> String -> String
caesarCipher bool string =
    String.foldr (cipherFolder bool) "" string


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



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


lines : List String -> List (Html Msg)
lines l =
    List.foldl (\a b -> b ++ [ p [] [ text a ] ]) [] l


encryptPoem : Poem -> Poem
encryptPoem poem =
    { poem | title = caesarCipher True poem.title, author = caesarCipher True poem.author, lines = [] }


decryptPoem : Poem -> Poem
decryptPoem poem =
    { poem | title = caesarCipher False poem.title, author = caesarCipher False poem.author, lines = [] }


showPoem : Maybe Poem -> List (Html Msg)
showPoem maybePoem =
    case maybePoem of
        Nothing ->
            [ h2 [] [ text "Click to load a poem" ] ]

        Just poem ->
            [ h2 [] [ text poem.title ], h3 [] [ text poem.author ], blockquote [] (lines poem.lines) ]


view : Model -> Html Msg
view model =
    let
        maybePoem =
            (List.head (List.drop model.selectedIndex model.poems))

        getPoemsButton =
            ([ button [ onClick GetPoems ] [ text "Load Poems" ] ])

        getRandomPoemButton =
            ([ div [] [ button [ onClick (GetRandomIndex maybePoem) ] [ text "Get Random Poem" ] ] ])

        encryptPoemButton =
            ([ div [] [ button [ disabled (model.isEncrypted), onClick (EncryptPeom maybePoem) ] [ text "Encrypt Poem" ] ] ])

        decryptPoemButton =
            ([ div [] [ button [ disabled (not model.isEncrypted), onClick (DecryptPeom maybePoem) ] [ text "Decrypt Poem" ] ] ])
    in
        div [ class "container application-container" ] ((showPoem maybePoem) ++ getRandomPoemButton ++ encryptPoemButton ++ decryptPoemButton)
