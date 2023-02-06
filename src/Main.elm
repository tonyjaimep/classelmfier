module Main exposing (..)

import Browser
import Canvas exposing (Point, Renderable, Shape, rect)
import Canvas.Settings exposing (fill, stroke)
import Canvas.Settings.Line exposing (lineDash, lineWidth)
import Color exposing (Color)
import Html exposing (Html, div, input, label, text)
import Html.Attributes exposing (style, type_, value)
import Html.Events exposing (onInput)
import Html.Events.Extra.Mouse as Mouse


type alias PointId =
    String


type alias Point2D =
    { id : PointId
    , x : Float
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


type alias Model =
    { weights : List Weight
    , points : List Point2D
    , graphDimensions : Dimensions
    }


type Msg
    = WeightValueChanged WeightId String
    | PointAdded Float Float
    | PointRemoved PointId
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
        [ { id = "bias"
          , value = 1.0
          }
        , { id = "w1"
          , value = 1.0
          }
        , { id = "w2"
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


setWeightValue : WeightId -> WeightValue -> List Weight -> List Weight
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

        PointAdded x y ->
            ( { model
                | points =
                    model.points ++ [ { id = "new id", x = x, y = y } ]
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


pointToCircle : Dimensions -> Point2D -> Shape
pointToCircle dimensions point =
    Canvas.circle
        ( toCanvasX dimensions.width point.x
        , toCanvasY dimensions.height point.y
        )
        pointSize


graphPoints : Dimensions -> List Point2D -> Renderable
graphPoints dimensions points =
    Canvas.shapes
        -- TODO: dynamic fill depending on weights
        [ fill Color.blue ]
        (List.map (pointToCircle dimensions) points)


modelLine : Dimensions -> List Weight -> Renderable
modelLine _ _ =
    Canvas.shapes [] []


graph : Dimensions -> List Point2D -> List Weight -> Html Msg
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
        , graphPoints dimensions points
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
