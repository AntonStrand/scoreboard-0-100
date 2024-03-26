port module Main exposing (main)

import Browser
import Html exposing (Html, button, div, h1, input, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Json.Encode as Encode


main : Program () Model Msg
main =
    Browser.document
        { init = \_ -> init
        , view = \model -> { title = "0-100", body = [ view model ] }
        , update = update
        , subscriptions = \_ -> Sub.none
        }



-- SETTINGS


numberOfQuestions : Int
numberOfQuestions =
    21


sectionLength : Int
sectionLength =
    numberOfQuestions // 3


correctGuessPoint : Int
correctGuessPoint =
    -10



-- PORTS


port storeState : String -> Cmd msg


encodeAnswers : List Answer -> Encode.Value
encodeAnswers =
    let
        encodeAnswer : Answer -> Encode.Value
        encodeAnswer { guess, correct, score } =
            Encode.object
                [ ( "guess", Encode.int guess )
                , ( "correct", Encode.int correct )
                , ( "score", Encode.int score )
                ]
    in
    Encode.list encodeAnswer


encodeCurrent : Current -> Encode.Value
encodeCurrent { guess, correct } =
    Encode.object
        [ ( "guess", Maybe.map Encode.int guess |> Maybe.withDefault Encode.null )
        , ( "correct", Maybe.map Encode.int correct |> Maybe.withDefault Encode.null )
        ]


encode : Model -> Encode.Value
encode { answers, current } =
    Encode.object
        [ ( "answers", encodeAnswers answers )
        , ( "current", encodeCurrent current )
        ]


saveState : Model -> Cmd msg
saveState model =
    storeState (model |> encode |> Encode.encode 0)



-- MODEL


type alias Answer =
    { guess : Int
    , correct : Int
    , score : Int
    }


type alias Current =
    { guess : Maybe Int
    , correct : Maybe Int
    }


type alias Model =
    { answers : List Answer
    , current : Current
    }


initialModel : Model
initialModel =
    { answers = []
    , current = initCurrent
    }


initCurrent : Current
initCurrent =
    { guess = Nothing
    , correct = Nothing
    }


init : ( Model, Cmd Msg )
init =
    ( initialModel
    , Cmd.none
    )



-- UPDATE


type Msg
    = SetGuess (Maybe Int)
    | SetCorrect (Maybe Int)
    | SaveAnswer
    | Restart
    | Noop


answer : Current -> Maybe Answer
answer { guess, correct } =
    let
        calcScore diff =
            if diff == 0 then
                correctGuessPoint

            else
                abs diff
    in
    Maybe.map2
        (\g c ->
            { guess = g
            , correct = c
            , score = calcScore (g - c)
            }
        )
        guess
        correct


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        limit =
            Maybe.map (clamp 0 100)

        newModel =
            case msg of
                SetGuess guess ->
                    { model | current = { guess = limit guess, correct = model.current.correct } }

                SetCorrect correct ->
                    { model | current = { guess = model.current.guess, correct = limit correct } }

                SaveAnswer ->
                    answer model.current
                        |> unwrap model (\a -> { model | answers = a :: model.answers, current = initCurrent })

                Restart ->
                    initialModel

                Noop ->
                    model
    in
    ( newModel, Cmd.batch [ saveState newModel ] )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ div [ class "header" ]
            [ h1 [] [ text "0-100" ]
            , button [ class "btn-secondary", onClick Restart ] [ text "Starta om" ]
            ]
        , viewAnswers model.answers
        , if List.length model.answers < numberOfQuestions then
            viewCurrent model.current

          else
            viewScore model.answers
        ]


viewScore : List Answer -> Html Msg
viewScore answered =
    div [ class "wrapper" ]
        [ h1 [] [ text ("Din slutgiltliga poäng: " ++ sumScore answered) ]
        , primary Restart "Ny omgång"
        ]


sumScore : List Answer -> String
sumScore =
    List.map .score >> List.sum >> String.fromInt


viewAnswers : List Answer -> Html msg
viewAnswers answers =
    let
        viewAnswer { guess, correct, score } =
            tr []
                [ td [] [ text (String.fromInt guess) ]
                , td [] [ text (String.fromInt correct) ]
                , td [] [ text (String.fromInt score) ]
                ]

        toSection sectionAnswers =
            List.map viewAnswer sectionAnswers
                ++ [ if List.length sectionAnswers < sectionLength then
                        text ""

                     else
                        tr [ class "sum" ]
                            [ th [] [ text "Delsumma" ]
                            , td [] []
                            , th [] [ text (sumScore sectionAnswers) ]
                            ]
                   ]

        orderedAnswers =
            List.reverse answers

        first =
            orderedAnswers |> List.take sectionLength |> toSection

        second =
            orderedAnswers |> List.drop sectionLength |> List.take sectionLength |> toSection

        third =
            orderedAnswers |> List.drop (sectionLength * 2) |> toSection
    in
    div [ class "wrapper" ]
        [ table []
            [ thead []
                [ th [] [ text "Ditt svar" ]
                , th [] [ text "Rätt svar" ]
                , th [] [ text "Poäng" ]
                ]
            , tbody [] (first ++ second ++ third)
            ]
        ]


viewCurrent : Current -> Html Msg
viewCurrent { guess, correct } =
    let
        txt ph =
            unwrap (placeholder ph) (String.fromInt >> value)
    in
    div [ id "answer", class "wrapper" ]
        [ input [ type_ "number", txt "Ditt svar" guess, onInput (String.toInt >> SetGuess) ] []
        , input [ type_ "number", txt "Rätt svar" correct, onInput (String.toInt >> SetCorrect) ] []
        , primary SaveAnswer "Svara"
        ]


primary : Msg -> String -> Html Msg
primary msg label =
    button [ class "btn-primary", onClick msg ] [ text label ]



-- HELPERS


unwrap : b -> (a -> b) -> Maybe a -> b
unwrap default f =
    Maybe.map f >> Maybe.withDefault default
