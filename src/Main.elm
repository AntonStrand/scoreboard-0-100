port module Main exposing (main)

import Browser
import Html exposing (Html, button, div, h1, input, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Json.Decode as Decode
import Json.Encode as Encode exposing (Value)


main : Program (Maybe Value) Model Msg
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


port storeState : String -> Cmd msg


saveState : Model -> Cmd msg
saveState =
    let
        encodeAnswer { guess, correct, score } =
            Encode.object
                [ ( "guess", Encode.int guess )
                , ( "correct", Encode.int correct )
                , ( "score", Encode.int score )
                ]

        encodeState state =
            case state of
                Guess guess ->
                    Encode.object
                        [ ( "tag", Encode.string "Guess" )
                        , ( "guess", unwrap Encode.null Encode.int guess )
                        ]

                Correct guess correct ->
                    Encode.object
                        [ ( "tag", Encode.string "Correct" )
                        , ( "guess", Encode.int guess )
                        , ( "correct", unwrap Encode.null Encode.int correct )
                        ]

        encodeModel { answers, state } =
            Encode.object
                [ ( "answers", Encode.list encodeAnswer answers )
                , ( "state", encodeState state )
                ]
    in
    encodeModel >> Encode.encode 0 >> storeState



-- MODEL


type alias Model =
    { answers : List Answer
    , state : GameState
    }


type alias Answer =
    { guess : Int
    , correct : Int
    , score : Int
    }


type GameState
    = Guess (Maybe Int)
    | Correct Int (Maybe Int)


initialModel : Model
initialModel =
    { answers = []
    , state = Guess Nothing
    }


init : Maybe Value -> ( Model, Cmd Msg )
init state =
    let
        decodeAnswer =
            Decode.map3 Answer
                (Decode.field "guess" Decode.int)
                (Decode.field "correct" Decode.int)
                (Decode.field "score" Decode.int)

        decodeState =
            Decode.field "tag" Decode.string
                |> Decode.andThen
                    (\tag ->
                        case tag of
                            "Guess" ->
                                Decode.map Guess (Decode.field "guess" (Decode.nullable Decode.int))

                            "Correct" ->
                                Decode.map2 Correct
                                    (Decode.field "guess" Decode.int)
                                    (Decode.field "correct" (Decode.nullable Decode.int))

                            invalidState ->
                                Decode.fail ("Invalid state: " ++ invalidState)
                    )

        decodeModel =
            Decode.map2 Model
                (Decode.field "answers" (Decode.list decodeAnswer))
                (Decode.field "state" decodeState)

        model =
            state
                |> Maybe.andThen (Decode.decodeValue Decode.string >> Result.toMaybe)
                |> Maybe.andThen (Decode.decodeString decodeModel >> Result.toMaybe)
                |> Maybe.withDefault initialModel
    in
    ( model
    , Cmd.none
    )



-- UPDATE


type Msg
    = SetGuess (Maybe Int)
    | SetCorrect Int (Maybe Int)
    | SaveGuess Int
    | SaveAnswer Int Int
    | Restart
    | Noop


answer : Int -> Int -> Answer
answer guess correct =
    let
        diff =
            abs (guess - correct)

        score =
            if diff == 0 then
                correctGuessPoint

            else
                diff
    in
    { guess = guess
    , correct = correct
    , score = score
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        limit =
            Maybe.map (clamp 0 100)

        newModel =
            case msg of
                SetGuess guess ->
                    { model | state = Guess (limit guess) }

                SaveGuess guess ->
                    { model | state = Correct guess Nothing }

                SetCorrect guess correct ->
                    { model | state = Correct guess (limit correct) }

                SaveAnswer guess correct ->
                    { model | state = Guess Nothing, answers = answer guess correct :: model.answers }

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
            viewCurrent model

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


viewCurrent : Model -> Html Msg
viewCurrent model =
    let
        txt ph =
            unwrap (placeholder ph) (String.fromInt >> value)
    in
    case model.state of
        Guess guess ->
            div [ id "answer", class "wrapper" ]
                [ input [ id "guess", type_ "number", txt "Ditt svar" guess, onInput (String.toInt >> SetGuess) ] []
                , primary (unwrap Noop SaveGuess guess) "Svara"
                ]

        Correct guess correct ->
            div [ id "answer", class "wrapper" ]
                [ text ("Ditt svar: " ++ String.fromInt guess)
                , input [ id "correct", type_ "number", txt "Rätt svar" correct, onInput (String.toInt >> SetCorrect guess) ] []
                , primary (unwrap Noop (SaveAnswer guess) correct) "Spara omgång"
                ]


primary : Msg -> String -> Html Msg
primary msg label =
    button [ class "btn-primary", onClick msg ] [ text label ]



-- HELPERS


unwrap : b -> (a -> b) -> Maybe a -> b
unwrap default f =
    Maybe.map f >> Maybe.withDefault default
