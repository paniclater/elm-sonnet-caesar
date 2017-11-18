module Main exposing (..)

import Html exposing (..)
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
    { name : String
    }


initModel : Model
initModel =
    Model ""


init : ( Model, Cmd Msg )
init =
    ( initModel
    , Cmd.none
    )



-- UPDATE


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


type alias Poem =
    { title : String
    , author : String
    , lines : List String
    }


sample : String
sample =
    """
{
 "title": "Not at home to callers",
 "author": "Emily Dickinson",
 "lines": [
      "Not at Home to Callers",
      "Says the Naked Tree --",
      "Bonnet due in April --",
      "Wishing you Good Day --"
 ]
}
"""


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


poem : Result String Poem -> List (Html Msg)
poem p =
    case p of
        Ok poem ->
            [ h2 [] [ text <| (++) "Title: " poem.title ], h3 [] [ text <| (++) "Author: " poem.author ], h4 [] (lines poem.lines) ]

        Err error ->
            [ span [] [ text error ] ]


view : Model -> Html Msg
view model =
    let
        decodedPoem =
            decodeString poemDecoder sample
    in
        div [] (poem decodedPoem)



-- [ poem decodedPoem ]            ]
