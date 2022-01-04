port module Main exposing (..)

import Browser
import Html exposing (Html, button, div, h1, input, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)


main : Program (Maybe Model) Model Msg
main =
    Browser.document
        { init = init
        , view = \model -> { title = "0-100", body = [ view model ] }
        , update = updateWithStorage
        , subscriptions = \_ -> Sub.none
        }


port setStorage : Model -> Cmd msg


{-| We want to `setStorage` on every update. This function adds the setStorage
command for every step of the update function.
-}
updateWithStorage : Msg -> Model -> ( Model, Cmd Msg )
updateWithStorage msg model =
    let
        newModel =
            update msg model
    in
    ( newModel
    , Cmd.batch [ setStorage newModel ]
    )


type alias Answered =
    { guess : Int
    , correct : Int
    , score : Int
    }


type alias Unanswered =
    { guess : Maybe Int
    , correct : Maybe Int
    }


type alias Model =
    { answered : List Answered
    , current : Unanswered
    , questions : List Unanswered
    }


initialModel : Model
initialModel =
    { answered = []
    , current = initUnanswered
    , questions = List.repeat 20 initUnanswered
    }


initUnanswered : Unanswered
initUnanswered =
    { guess = Nothing
    , correct = Nothing
    }


init : Maybe Model -> ( Model, Cmd Msg )
init maybeModel =
    ( Maybe.withDefault initialModel maybeModel
    , Cmd.none
    )


unwrap : b -> (a -> b) -> Maybe a -> b
unwrap default f =
    Maybe.map f >> Maybe.withDefault default


type Msg
    = SetGuess (Maybe Int)
    | SetCorrect (Maybe Int)
    | Answer
    | Restart
    | Noop


answer : Unanswered -> Maybe Answered
answer { guess, correct } =
    Maybe.map2
        (\g c ->
            { guess = g
            , correct = c
            , score =
                if c - g == 0 then
                    -10

                else
                    abs (c - g)
            }
        )
        guess
        correct


update : Msg -> Model -> Model
update msg model =
    let
        limit =
            Maybe.map (clamp 0 100)
    in
    case msg of
        SetGuess guess ->
            { model | current = { guess = limit guess, correct = model.current.correct } }

        SetCorrect correct ->
            { model | current = { guess = model.current.guess, correct = limit correct } }

        Answer ->
            answer model.current
                |> unwrap model (\a -> { model | answered = a :: model.answered, current = initUnanswered })

        Restart ->
            initialModel

        Noop ->
            model


view : Model -> Html Msg
view model =
    div []
        [ viewAnswered model.answered
        , if List.length model.answered < 21 then
            viewCurrent model.current

          else
            viewScore model.answered
        ]


viewScore : List Answered -> Html Msg
viewScore answered =
    div []
        [ h1 [] [ text ("Din slutgiltliga poäng: " ++ sumScore answered) ]
        , button [ onClick Restart ] [ text "Ny omgång" ]
        ]


sumScore : List Answered -> String
sumScore =
    List.map .score >> List.sum >> String.fromInt


viewAnswered : List Answered -> Html msg
viewAnswered answered =
    let
        viewAnswer { guess, correct, score } =
            tr []
                [ td [] [ text (String.fromInt guess) ]
                , td [] [ text (String.fromInt correct) ]
                , td [] [ text (String.fromInt score) ]
                ]

        toSection answers =
            List.map viewAnswer answers
                ++ [ if List.length answers < 7 then
                        text ""

                     else
                        tr [ class "sum" ]
                            [ th [] [ text "Delsumma" ]
                            , td [] []
                            , th [] [ text (sumScore answers) ]
                            ]
                   ]

        first =
            answered |> List.reverse |> List.take 7 |> toSection

        second =
            answered |> List.reverse |> List.drop 7 |> List.take 7 |> toSection

        third =
            answered |> List.reverse |> List.drop 14 |> toSection
    in
    table []
        [ thead []
            [ th [] [ text "Gissning" ]
            , th [] [ text "Rätt svar" ]
            , th [] [ text "Poäng" ]
            ]
        , tbody [] (first ++ second ++ third)
        ]


viewCurrent : Unanswered -> Html Msg
viewCurrent { guess, correct } =
    let
        txt ph =
            unwrap (placeholder ph) (String.fromInt >> value)

        on msg input =
            if String.isEmpty input then
                msg Nothing

            else
                String.toInt input |> unwrap Noop (Just >> msg)
    in
    div []
        [ input [ txt "Din gissning" guess, onInput (on SetGuess) ] []
        , input [ txt "Rätt svar" correct, onInput (on SetCorrect) ] []
        , button [ onClick Answer ] [ text "Klar" ]
        ]
