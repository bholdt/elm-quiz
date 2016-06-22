module Main exposing (..)

import Html exposing (Html, button, div, text)
import Html.App as App
import Html.Events exposing (onClick)
import List.Extra
import Task exposing (..)
import Maybe
import Process

main =
    App.program
        { init = ( initModel, fetchQuestions )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


type alias Answer = {
    text: String
  , correct: Bool
}

type alias Question = {
     id: Int
   , text: String
   , answers: List Answer
   , points: Int
}

type alias Model = {
    questions: List Question
  , answeredQuestions: List Question
  , currentQuestion: Maybe Question
  , score: Int
}

initModel : Model
initModel = {
     questions = []
   , answeredQuestions = []
   , currentQuestion = Nothing
   , score = 0
  }

type Msg = QuestionResponse (List Question) | PickQuestion (Maybe Question) | AnswerQuestion Answer


createNew: Model -> Model
createNew model =  
    case model.currentQuestion of
        Just c ->
            let
                newAnswered = c :: model.answeredQuestions
                newQuestions = List.Extra.deleteIf (\a -> a.id == c.id) model.questions
            in 
                { model | answeredQuestions = newAnswered, questions = newQuestions } 
        Nothing -> 
            model

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        QuestionResponse questions -> 
            ({ model | questions = questions }, pickRandomQuestion questions )
        PickQuestion question -> 
            ({ model | currentQuestion = question }, Cmd.none)
        AnswerQuestion answer ->
            let
                newModel = createNew model
            in
                if answer.correct then
                    ({ newModel | score = newModel.score + 1 }, pickRandomQuestion newModel.questions)
                else 
                    (newModel, pickRandomQuestion newModel.questions)

pickRandomQuestion: List Question -> Cmd Msg
pickRandomQuestion questions = 
    let
        nextQuestion = List.head questions
        getQuestion = Task.fromResult (Ok nextQuestion)
    in
        Task.perform PickQuestion PickQuestion getQuestion

fetchQuestions: Cmd Msg
fetchQuestions =
    let
        questions = [ 
                { id = 1, text = "Capital of Namibia?", answers = [{text = "WIndhoek", correct = True}], points = 1 }
            ]
    in
        Task.perform QuestionResponse QuestionResponse (Process.sleep 2000 `andThen` (\_ -> Task.fromResult (Ok questions)))

viewAnswer: Answer -> Html Msg
viewAnswer answer =
    div [] [
        button [ onClick (AnswerQuestion answer) ] [ text answer.text ]
    ]

viewQuestion: Question -> Html Msg
viewQuestion question =
    div [] [
        text question.text
      , div [] (List.map viewAnswer question.answers)
    ]

view : Model -> Html Msg
view model =
    case model.currentQuestion of
        Nothing -> 
            div [] [ text "Total points:", text (toString model.score) ]
        Just question ->
            div [] [ viewQuestion question ]
