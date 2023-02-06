module Main exposing (..)

import Browser
import Canvas exposing (Renderable, Shape)
import Canvas.Settings exposing (fill, stroke)
import Canvas.Settings.Line exposing (lineDash, lineWidth)
import Color exposing (Color)
import Html exposing (Html, div, input, label, text)
import Html.Attributes exposing (style, type_, value)
import Html.Events exposing (onInput)
import Html.Events.Extra.Mouse as Mouse


type alias ActivationFunction =
    Float -> Float


type alias DataPointId =
    String


type alias DataPoint =
    { id : DataPointId
    , x1 : Float
    , x2 : Float
    , y : Float
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
    , points : List DataPoint
    , graphDimensions : Dimensions
    }


type Msg
    = WeightValueChanged WeightId String
    | PointAdded Float Float
    | PointRemoved DataPointId
    | CanvasClicked ( Float, Float )
    | WeightsChanged



-- constants


pointSize : Float
pointSize =
    3.0


canvasBackground : Color
canvasBackground =
    Color.white


weightEditor : Weight -> Html Msg
weightEditor weight =
    div
        [ style "display" "flex"
        , style "flex-direction" "column"
        ]
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
        [ style "display" "flex"
        , style "justify-content" "space-between"
        ]
        (List.map weightEditor model.weights)


view : Model -> Html Msg
view model =
    div
        [ style "display" "flex"
        , style "flex-direction" "column"
        , style "justify-content" "stretch"
        , style "background-color" "#fffff0"
        , style "height" "100%"
        ]
        [ graph model.graphDimensions model.points model.weights
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
    , points =
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

        PointAdded x1 x2 ->
            let
                point =
                    { id = "new id"
                    , x1 = x1
                    , x2 = x2
                    , y = 0
                    }
            in
            ( { model
                | points =
                    model.points
                        ++ [ { point
                                | y = calculateValue model.weights point
                             }
                           ]
              }
            , Cmd.none
            )

        PointRemoved pointId ->
            ( { model
                | points =
                    List.filter (\point -> point.id /= pointId) model.points
              }
            , Cmd.none
            )

        CanvasClicked ( x, y ) ->
            update
                (PointAdded
                    (toGraphX model.graphDimensions.width x)
                    -- event y positions are top-to-bottom
                    -- we want bottom-to-top
                    (toGraphY model.graphDimensions.height y)
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
    { title = "Classelmfier"
    , body = [ view model ]
    }


canvasGraphLines : Dimensions -> Renderable
canvasGraphLines dimensions =
    Canvas.shapes
        [ stroke (Color.rgba 0 0 0 0.7)
        , lineWidth 1
        , lineDash [ 5 ]
        ]
        [ Canvas.path
            ( toFloat dimensions.width / 2, 0 )
            [ Canvas.lineTo ( toFloat dimensions.width / 2, toFloat dimensions.height ) ]
        , Canvas.path
            ( toFloat 0, toFloat dimensions.height / 2 )
            [ Canvas.lineTo ( toFloat dimensions.width, toFloat dimensions.height / 2 ) ]
        ]


pointToCircle : Dimensions -> DataPoint -> Shape
pointToCircle dimensions point =
    Canvas.circle
        ( toCanvasX dimensions.width point.x1
        , toCanvasY dimensions.height point.x2
        )
        pointSize


stepActivation : ActivationFunction
stepActivation x =
    if x > 0 then
        1

    else
        0


graphPoints : Weights -> Dimensions -> List DataPoint -> Renderable
graphPoints weights dimensions points =
    Canvas.group
        []
        (List.map
            (\point ->
                Canvas.shapes
                    [ fill (pointColor stepActivation weights point) ]
                    [ pointToCircle dimensions point ]
            )
            points
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


disabledPointRgb : Rgb
disabledPointRgb =
    { r = 0.3
    , g = 0.2
    , b = 1.0
    }


enabledPointRgb : Rgb
enabledPointRgb =
    { r = 1.0
    , g = 0.2
    , b = 0.0
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


calculateValue : Weights -> DataPoint -> Float
calculateValue weights dataPoint =
    List.foldl
        (+)
        0
        (List.map2
            (*)
            [ dataPoint.x1, dataPoint.x2, 1 ]
            (weightList weights)
        )


pointColor : ActivationFunction -> Weights -> DataPoint -> Color
pointColor activationFunction weights point =
    calculateValue weights point
        |> activationFunction
        |> colorBetween disabledPointRgb enabledPointRgb
        |> rgbToColor


modelLine : Dimensions -> Weights -> Renderable
modelLine dimensions weights =
    Canvas.shapes
        [ stroke Color.red
        , lineWidth 2
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


graph : Dimensions -> List DataPoint -> Weights -> Html Msg
graph dimensions points weights =
    Canvas.toHtml ( dimensions.width, dimensions.height )
        [ style "border" "1px solid black"
        , style "display" "block"
        , style "margin" "0 auto"
        , Mouse.onClick (\event -> CanvasClicked event.offsetPos)
        ]
        [ Canvas.shapes
            [ fill canvasBackground ]
            [ Canvas.rect
                ( 0, 0 )
                (toFloat dimensions.width)
                (toFloat dimensions.height)
            ]
        , canvasGraphLines dimensions
        , graphPoints weights dimensions points
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
