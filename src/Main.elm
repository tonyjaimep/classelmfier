module Main exposing (..)

import Browser
import Canvas exposing (Renderable, Shape)
import Canvas.Settings exposing (fill, stroke)
import Canvas.Settings.Line exposing (lineDash, lineWidth)
import Color exposing (Color)
import Html exposing (Html, aside, button, div, input, label, text)
import Html.Attributes exposing (class, readonly, style, type_, value)
import Html.Events exposing (onClick, onInput)
import Html.Events.Extra.Mouse as Mouse
import Random
import Time


type alias ActivationFunction =
    Float -> Float


type alias NetworkOutput =
    Float


type alias Network =
    DataPoint -> NetworkOutput


type alias DataPointId =
    String


type alias DataPoint =
    { id : DataPointId
    , x1 : Float
    , x2 : Float
    , expectedOutput : NetworkOutput
    }


type alias WeightId =
    String


type alias WeightValue =
    Float


type alias Weight =
    { id : WeightId
    , value : WeightValue
    }


type alias Dimensions =
    { width : Int
    , height : Int
    }


type alias Weights =
    List Weight


type alias Model =
    { weights : Weights
    , dataPoints : List DataPoint
    , graphDimensions : Dimensions
    , activation : ActivationFunction
    , isTraining : Bool
    , epochs : Int
    , nextExpectedOutput : Float
    }


type Msg
    = WeightValueChanged WeightId String
    | WeightValueSet WeightId WeightValue
    | DataPointAdded DataPoint
    | DataPointRemoved DataPointId
    | DataPointTypeChanged Float
    | CanvasClicked ( Float, Float )
    | DataPointsCleared
    | Epoch
    | TrainingStarted
    | TrainingStopped
    | TrainingTick


pointSize : Float
pointSize =
    3.0


learningRate : Float
learningRate =
    0.4


epochLimit : Int
epochLimit =
    500


canvasSize : Int
canvasSize =
    500


canvasBackground : Color
canvasBackground =
    Color.black


weightEditor : Weight -> Html Msg
weightEditor weight =
    div
        [ class "weight-editor" ]
        [ label
            []
            [ text weight.id ]
        , input
            [ type_ "number"
            , readonly True
            , value (String.fromFloat weight.value)
            , onInput (WeightValueChanged weight.id)
            ]
            []
        ]


controls : Model -> Html Msg
controls model =
    div
        [ class "weight-controls" ]
        (List.map weightEditor model.weights)


neuronSigma : Model -> DataPoint -> Float
neuronSigma model dataPoint =
    List.foldl
        (+)
        0
        (List.map2 (*) [ dataPoint.x1, dataPoint.x2, 1 ] (weightValues model.weights))


networkForModel : Model -> Network
networkForModel model input =
    neuronSigma model input
        |> model.activation


view : Model -> Html Msg
view model =
    let
        network =
            networkForModel model
    in
    div
        [ class "view-container" ]
        [ div
            [ class "network-data-container" ]
            [ graph model network
            , sidebar model network
            ]
        , controls model
        ]


initialModel : Model
initialModel =
    { weights =
        [ { id = "w1"
          , value = 1.0
          }
        , { id = "w2"
          , value = 1.0
          }
        , { id = "bias"
          , value = 1.0
          }
        ]
    , dataPoints =
        []
    , graphDimensions =
        { width = canvasSize
        , height = canvasSize
        }
    , activation = stepActivation
    , isTraining = False
    , epochs = 0
    , nextExpectedOutput = 1
    }


setWeightValue : WeightId -> WeightValue -> Weights -> Weights
setWeightValue weightId weightValue weights =
    let
        replaceWeightValue weight =
            if weight.id == weightId then
                { weight | value = weightValue }

            else
                weight
    in
    List.map replaceWeightValue weights


networkErrors : Network -> List DataPoint -> List Float
networkErrors network dataPoints =
    List.map2
        (\expected -> \output -> expected - output)
        (List.map .expectedOutput dataPoints)
        (List.map network dataPoints)


adjustedWeight : Float -> Weight -> Float -> Weight
adjustedWeight error weight dataDimension =
    { weight
        | value =
            weight.value + (dataDimension * error * learningRate)
    }


adjustedWeights : DataPoint -> Model -> Weights
adjustedWeights dataPoint model =
    let
        network =
            networkForModel model

        error =
            dataPoint.expectedOutput - network dataPoint
    in
    List.map2
        (adjustedWeight error)
        model.weights
        (dataPointVector dataPoint)


modelWithAdjustedWeights : Model -> Model
modelWithAdjustedWeights model =
    let
        adjustedForDataPoint dataPoint model_ =
            { model_ | weights = adjustedWeights dataPoint model_ }
    in
    List.foldl adjustedForDataPoint model model.dataPoints


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        WeightValueSet id value ->
            ( { model
                | weights =
                    setWeightValue id value model.weights
              }
            , Cmd.none
            )

        WeightValueChanged id valueAsString ->
            let
                parsedValue =
                    case String.toFloat valueAsString of
                        Maybe.Just value ->
                            value

                        Maybe.Nothing ->
                            0.0
            in
            update (WeightValueSet id parsedValue) model

        DataPointAdded input ->
            ( { model | dataPoints = model.dataPoints ++ [ input ] }
            , Cmd.none
            )

        DataPointRemoved inputId ->
            ( { model
                | dataPoints =
                    List.filter (\input -> input.id /= inputId) model.dataPoints
              }
            , Cmd.none
            )

        DataPointsCleared ->
            ( { model | dataPoints = [] }, Cmd.none )

        DataPointTypeChanged nextExpectedOutput ->
            ( { model | nextExpectedOutput = nextExpectedOutput }, Cmd.none )

        CanvasClicked ( x, y ) ->
            update
                (DataPointAdded
                    { id = "algo"
                    , x1 = toGraphX model.graphDimensions.width x
                    , x2 = toGraphY model.graphDimensions.height y
                    , expectedOutput = model.nextExpectedOutput
                    }
                )
                model

        Epoch ->
            let
                adjusted =
                    modelWithAdjustedWeights model
            in
            ( { adjusted | epochs = adjusted.epochs + 1 }
            , Cmd.none
            )

        TrainingTick ->
            if model.epochs < epochLimit then
                update Epoch model

            else
                ( { model | isTraining = False }, Cmd.none )

        TrainingStarted ->
            ( { model | isTraining = True, epochs = 0 }, Cmd.none )

        TrainingStopped ->
            ( { model | isTraining = False }, Cmd.none )


type alias GraphicValueTransformer =
    Int -> Float -> Float


toGraphRelative : GraphicValueTransformer
toGraphRelative graphDimension value =
    let
        zeroPosition =
            Basics.toFloat graphDimension / 2
    in
    value - zeroPosition


toGraphX : GraphicValueTransformer
toGraphX =
    toGraphRelative


toGraphY : GraphicValueTransformer
toGraphY graphHeight y =
    -1 * toGraphRelative graphHeight y


toCanvasRelative : GraphicValueTransformer
toCanvasRelative graphDimension value =
    Basics.toFloat graphDimension / 2 + value


toCanvasX : Int -> Float -> Float
toCanvasX graphDimension value =
    toCanvasRelative graphDimension value


toCanvasY : Int -> Float -> Float
toCanvasY graphDimension value =
    toCanvasRelative graphDimension (-1 * value)


documentView : Model -> Browser.Document Msg
documentView model =
    { title = "Classelmfier"
    , body = [ view model ]
    }


canvasGraphLines : Dimensions -> Renderable
canvasGraphLines dimensions =
    Canvas.shapes
        [ stroke (Color.rgba 1 1 1 0.7)
        , lineWidth 0
        , lineDash [ 4, 6 ]
        ]
        [ Canvas.path
            ( toFloat dimensions.width / 2
            , toFloat dimensions.height / 2
            )
            [ Canvas.lineTo
                ( toFloat dimensions.width / 2
                , toFloat dimensions.height
                )
            ]
        , Canvas.path
            ( toFloat dimensions.width / 2
            , toFloat dimensions.height / 2
            )
            [ Canvas.lineTo
                ( toFloat dimensions.width / 2
                , 0
                )
            ]
        , Canvas.path
            ( toFloat dimensions.width / 2
            , toFloat dimensions.height / 2
            )
            [ Canvas.lineTo
                ( 0
                , toFloat dimensions.height / 2
                )
            ]
        , Canvas.path
            ( toFloat dimensions.width / 2
            , toFloat dimensions.height / 2
            )
            [ Canvas.lineTo
                ( toFloat dimensions.width
                , toFloat dimensions.height / 2
                )
            ]
        ]


inputToShape : Dimensions -> DataPoint -> Shape
inputToShape dimensions input =
    Canvas.circle
        ( toCanvasX dimensions.width input.x1
        , toCanvasY dimensions.height input.x2
        )
        pointSize


stepActivation : ActivationFunction
stepActivation x =
    if x > 0 then
        1

    else
        0


sigmoidActivation : ActivationFunction
sigmoidActivation x =
    1 / (1 + (e ^ (-0.01 * x)))


graphPoints : Network -> Dimensions -> List DataPoint -> Renderable
graphPoints network dimensions dataPoints =
    Canvas.group
        [ lineWidth (pointSize / 2) ]
        (List.map
            (\dataPoint ->
                Canvas.shapes
                    [ dataPoint
                        |> network
                        |> networkOutputColor
                        |> fill
                    , dataPoint.expectedOutput
                        |> networkOutputColor
                        |> stroke
                    ]
                    [ inputToShape dimensions dataPoint ]
            )
            dataPoints
        )


firstWeightValue : Weights -> Float
firstWeightValue weights =
    case List.head weights of
        Maybe.Just weight ->
            weight.value

        Maybe.Nothing ->
            0


secondWeight : Weights -> Float
secondWeight weights =
    case List.tail weights of
        Maybe.Just weightsTail ->
            case List.head weightsTail of
                Maybe.Just weight ->
                    weight.value

                Maybe.Nothing ->
                    0

        Maybe.Nothing ->
            0


biasValue : Weights -> Float
biasValue weights =
    case List.head (List.reverse weights) of
        Maybe.Just weight ->
            weight.value

        Maybe.Nothing ->
            0


calculateY : Float -> Weights -> Float
calculateY x weights =
    let
        dividedBySecondWeight value =
            value / secondWeight weights
    in
    -1
        * (dividedBySecondWeight (firstWeightValue weights)
            * x
            + dividedBySecondWeight (biasValue weights)
          )


type alias Rgb =
    { r : Float
    , g : Float
    , b : Float
    }


disabledOutputRgb : Rgb
disabledOutputRgb =
    { r = 0.3
    , g = 0.2
    , b = 1.0
    }


enabledOutputRgb : Rgb
enabledOutputRgb =
    { r = 1.0
    , g = 0.8
    , b = 0.3
    }


colorBetween : Rgb -> Rgb -> Float -> Rgb
colorBetween from to percent =
    { r = from.r + percent * (to.r - from.r)
    , g = from.g + percent * (to.g - from.g)
    , b = from.b + percent * (to.b - from.b)
    }


rgbToColor : Rgb -> Color
rgbToColor rgb =
    Color.rgb rgb.r rgb.g rgb.b


weightValues : Weights -> List Float
weightValues weights =
    List.map .value weights


weightDelta : Float -> NetworkOutput -> Float
weightDelta error output =
    learningRate * error * output


dataPointVector : DataPoint -> List Float
dataPointVector dataPoint =
    [ dataPoint.x1, dataPoint.x2, 1 ]


networkOutputColor : NetworkOutput -> Color
networkOutputColor output =
    output
        |> colorBetween disabledOutputRgb enabledOutputRgb
        |> rgbToColor


modelLine : Dimensions -> Weights -> Renderable
modelLine dimensions weights =
    Canvas.shapes
        [ stroke Color.red
        , lineWidth 1
        ]
        [ Canvas.path
            ( 0
            , toCanvasY dimensions.height (calculateY (toGraphX dimensions.width 0) weights)
            )
            [ Canvas.lineTo
                ( toFloat dimensions.width
                , toCanvasY
                    dimensions.height
                    (calculateY
                        (toGraphRelative
                            dimensions.width
                            (toFloat dimensions.width)
                        )
                        weights
                    )
                )
            ]
        ]


outputColorCircleHtml : NetworkOutput -> NetworkOutput -> Html Msg
outputColorCircleHtml output expectedOutput =
    div
        [ class "network-output-circle"
        , output
            |> networkOutputColor
            |> Color.toCssString
            |> style "background-color"
        , expectedOutput
            |> networkOutputColor
            |> Color.toCssString
            |> style "border-color"
        ]
        []


inputDetail : Network -> DataPoint -> Html Msg
inputDetail network dataPoint =
    let
        output =
            network dataPoint
    in
    div
        [ class "data-point-detail" ]
        [ outputColorCircleHtml output dataPoint.expectedOutput
        , text "("
        , dataPoint.x1
            |> String.fromFloat
            |> text
        , text ","
        , dataPoint.x2
            |> String.fromFloat
            |> text
        , text ")"
        ]


inputClearingButton : Html Msg
inputClearingButton =
    button
        [ onClick DataPointsCleared ]
        [ text "Clear data points" ]


startTrainingButton : Bool -> Html Msg
startTrainingButton isTraining =
    let
        message =
            if isTraining then
                TrainingStopped

            else
                TrainingStarted

        textString =
            if isTraining then
                "Stop training"

            else
                "Start training"
    in
    button
        [ onClick message ]
        [ text textString ]


dataPointListing : List DataPoint -> Network -> Html Msg
dataPointListing dataPoints network =
    div
        [ class "data-point-listing" ]
        (List.map
            (inputDetail network)
            dataPoints
        )


inputControls : List DataPoint -> Html Msg
inputControls dataPoints =
    if List.length dataPoints > 0 then
        inputClearingButton

    else
        Html.text ""


expectedOutputControl : Model -> Html Msg
expectedOutputControl model =
    div [ class "expected-output-control" ]
        [ label
            [ class "expected-output-label"
            ]
            [ text "Expect"
            , outputColorCircleHtml model.nextExpectedOutput model.nextExpectedOutput
            , text "("
            , model.nextExpectedOutput
                |> String.fromFloat
                |> text
            , text ")"
            ]
        , input
            [ type_ "range"
            , Html.Attributes.min "0"
            , Html.Attributes.max "1"
            , Html.Attributes.step "1"
            , onInput
                (\v ->
                    String.toFloat v
                        |> Maybe.withDefault 0.3
                        |> DataPointTypeChanged
                )
            ]
            []
        ]


sidebar : Model -> Network -> Html Msg
sidebar model network =
    aside
        []
        [ expectedOutputControl model
        , startTrainingButton model.isTraining
        , inputControls model.dataPoints
        , dataPointListing model.dataPoints network
        ]


graphCanvasContent : Model -> Network -> List Canvas.Renderable
graphCanvasContent model network =
    [ Canvas.shapes
        [ fill canvasBackground ]
        [ Canvas.rect
            ( 0, 0 )
            (toFloat model.graphDimensions.width)
            (toFloat model.graphDimensions.height)
        ]
    , canvasGraphLines model.graphDimensions
    , graphPoints network model.graphDimensions model.dataPoints
    , modelLine model.graphDimensions model.weights
    ]


graph : Model -> Network -> Html Msg
graph model network =
    div
        [ class "graph-container" ]
        [ Canvas.toHtml
            ( model.graphDimensions.width
            , model.graphDimensions.height
            )
            [ Mouse.onClick (\event -> CanvasClicked event.offsetPos) ]
            (graphCanvasContent model network)
        ]


randomWeightValueGenerator : Random.Generator WeightValue
randomWeightValueGenerator =
    Random.float
        (-0.5 * toFloat canvasSize)
        (0.5 * toFloat canvasSize)


randomWeightsTasks : Model -> List (Cmd Msg)
randomWeightsTasks model =
    let
        taskForWeight weight =
            Random.generate (WeightValueSet weight.id) randomWeightValueGenerator
    in
    List.map taskForWeight model.weights


init : flags -> ( Model, Cmd Msg )
init _ =
    ( initialModel
    , Cmd.batch
        (randomWeightsTasks initialModel)
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.isTraining then
        Time.every 100 (\_ -> TrainingTick)

    else
        Sub.none


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = documentView
        , update = update
        , subscriptions = subscriptions
        }
