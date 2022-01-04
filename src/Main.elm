port module Main exposing (main)

import Browser
import Html exposing (Html, button, div, h1, input, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)


main : Program (Maybe Model) Model Msg
main =
    Browser.document
        { init = init
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


port saveState : Model -> Cmd msg



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


init : Maybe Model -> ( Model, Cmd Msg )
init maybeModel =
    ( Maybe.withDefault initialModel maybeModel
    , Cmd.none
    )



-- UPDATE


type Msg
    = SetGuess Int
    | SetCorrect Int
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
            Just << clamp 0 100

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
        [ viewAnswers model.answers
        , if List.length model.answers < numberOfQuestions then
            viewCurrent model.current

          else
            viewScore model.answers
        ]


viewScore : List Answer -> Html Msg
viewScore answered =
    div []
        [ h1 [] [ text ("Din slutgiltliga poäng: " ++ sumScore answered) ]
        , button [ onClick Restart ] [ text "Ny omgång" ]
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
    table []
        [ thead []
            [ th [] [ text "Ditt svar" ]
            , th [] [ text "Rätt svar" ]
            , th [] [ text "Poäng" ]
            ]
        , tbody [] (first ++ second ++ third)
        ]


viewCurrent : Current -> Html Msg
viewCurrent { guess, correct } =
    let
        txt ph =
            unwrap (placeholder ph) (String.fromInt >> value)

        on msg =
            String.toInt >> unwrap Noop msg
    in
    div []
        [ input [ type_ "number", txt "Ditt svar" guess, onInput (on SetGuess) ] []
        , input [ type_ "number", txt "Rätt svar" correct, onInput (on SetCorrect) ] []
        , button [ onClick SaveAnswer ] [ text "Klar" ]
        , div [] [ button [ onClick Restart ] [ text "Starta om" ] ]
        ]



-- HELPERS


unwrap : b -> (a -> b) -> Maybe a -> b
unwrap default f =
    Maybe.map f >> Maybe.withDefault default
