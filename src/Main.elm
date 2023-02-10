module Main exposing (..)

import Browser
import Canvas exposing (Renderable, Shape)
import Canvas.Settings exposing (fill, stroke)
import Canvas.Settings.Line exposing (lineDash, lineWidth)
import Color exposing (Color)
import Html exposing (Html, div, input, label, text)
import Html.Attributes exposing (class, style, type_, value)
import Html.Events exposing (onInput)
import Html.Events.Extra.Mouse as Mouse


type alias ActivationFunction =
    Float -> Float


type alias NetworkOutput =
    Float


type alias Network =
    NetworkInput -> NetworkOutput


type alias NetworkInputId =
    String


type alias NetworkInput =
    { id : NetworkInputId
    , x1 : Float
    , x2 : Float
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



-- FIXME: model only has one neuron. Weights should be defined per neuron


type alias Model =
    { weights : Weights
    , inputs : List NetworkInput
    , graphDimensions : Dimensions
    }


type Msg
    = WeightValueChanged WeightId String
    | NetworkInputAdded NetworkInput
    | NetworkInputRemoved NetworkInputId
    | CanvasClicked ( Float, Float )
    | WeightsChanged


pointSize : Float
pointSize =
    3.0


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


neuronSigma : Model -> NetworkInput -> Float
neuronSigma model input =
    List.foldl
        (+)
        0
        (List.map2 (*) [ input.x1, input.x2, 1 ] (weightList model.weights))


networkForModel : Model -> ActivationFunction -> Network
networkForModel model activation input =
    neuronSigma model input
        |> activation


view : Model -> Network -> Html Msg
view model network =
    div
        [ class "view-container" ]
        [ div
            [ class "network-data-container" ]
            [ graph model.graphDimensions model.inputs network model.weights
            , inputListing model.inputs network
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
    , inputs =
        []
    , graphDimensions =
        { width = 500
        , height = 500
        }
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        WeightValueChanged id valueAsString ->
            let
                parsedValue =
                    case String.toFloat valueAsString of
                        Maybe.Just value ->
                            value

                        Maybe.Nothing ->
                            0.0
            in
            ( { model
                | weights =
                    setWeightValue id parsedValue model.weights
              }
            , Cmd.none
            )

        NetworkInputAdded input ->
            ( { model | inputs = model.inputs ++ [ input ] }
            , Cmd.none
            )

        NetworkInputRemoved inputId ->
            ( { model
                | inputs =
                    List.filter (\input -> input.id /= inputId) model.inputs
              }
            , Cmd.none
            )

        CanvasClicked ( x, y ) ->
            update
                (NetworkInputAdded
                    { id = "algo"
                    , x1 = toGraphX model.graphDimensions.width x

                    -- event y positions are top-to-bottom
                    -- we want bottom-to-top
                    , x2 = toGraphY model.graphDimensions.height y
                    }
                )
                model

        WeightsChanged ->
            ( model, Cmd.none )


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



-- assuming proportion is 1:1


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
    let
        network =
            networkForModel model sigmoidActivation
    in
    { title = "Classelmfier"
    , body = [ view model network ]
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


inputToShape : Dimensions -> NetworkInput -> Shape
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


graphPoints : Network -> Dimensions -> List NetworkInput -> Renderable
graphPoints network dimensions inputs =
    Canvas.group
        []
        (List.map
            (\input ->
                Canvas.shapes
                    [ input
                        |> network
                        |> networkOutputColor
                        |> fill
                    ]
                    [ inputToShape dimensions input ]
            )
            inputs
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


weightList : Weights -> List Float
weightList weights =
    List.map .value weights


calculateValue : Weights -> NetworkInput -> NetworkOutput
calculateValue weights dataPoint =
    List.foldl
        (+)
        0
        (List.map2
            (*)
            [ dataPoint.x1, dataPoint.x2, 1 ]
            (weightList weights)
        )


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


outputColorCircleHtml : NetworkOutput -> Html Msg
outputColorCircleHtml output =
    div
        [ class "network-output-circle"
        , output
            |> networkOutputColor
            |> Color.toCssString
            |> style "background-color"
        ]
        []


inputDetail : Network -> NetworkInput -> Html Msg
inputDetail network networkInput =
    let
        output =
            network networkInput
    in
    div
        [ class "network-input-detail" ]
        [ outputColorCircleHtml output
        , text "("
        , networkInput.x1
            |> String.fromFloat
            |> text
        , text ","
        , networkInput.x2
            |> String.fromFloat
            |> text
        , text ")"
        ]


inputListing : List NetworkInput -> Network -> Html Msg
inputListing inputs network =
    div
        [ class "network-input-listing" ]
        (List.map (inputDetail network) inputs)


graph : Dimensions -> List NetworkInput -> Network -> Weights -> Html Msg
graph dimensions inputs network weights =
    Canvas.toHtml ( dimensions.width, dimensions.height )
        [ Mouse.onClick (\event -> CanvasClicked event.offsetPos) ]
        [ Canvas.shapes
            [ fill canvasBackground ]
            [ Canvas.rect
                ( 0, 0 )
                (toFloat dimensions.width)
                (toFloat dimensions.height)
            ]
        , canvasGraphLines dimensions
        , graphPoints network dimensions inputs
        , modelLine dimensions weights
        ]


main : Program () Model Msg
main =
    Browser.document
        { init = \_ -> ( initialModel, Cmd.none )
        , view = documentView
        , update = update
        , subscriptions = \_ -> Sub.none
        }
