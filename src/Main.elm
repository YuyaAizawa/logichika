port module Main exposing (main)

import Browser
import Canvas exposing (Point, Renderable, shapes, rect)
import Canvas.Settings exposing (fill)
import Color
import Dict exposing (Dict)
import Html exposing (Attribute, Html, div, input, label, text)
import Html.Attributes as Attr
import Html.Events exposing (onClick)
import Json.Decode as Json


main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }



-----------
-- MODEL --
-----------

type alias Coords = ( Int, Int )

type Element
  = Wire
  | Bridge
  | Button
  | Input
  | And
  | Nor

type Tool  -- マウスに割当たっている状態，みたいな名前に変える
  = Drawer Element
  | Eraser

type alias Model =
  { grid : Dict Coords Element
  , tool : Tool
  , zoom : Int
  }

init : () -> ( Model, Cmd msg )
init _ =
  (
    { grid = Dict.empty
    , tool = Drawer Wire
    , zoom = 8
    }
  , Cmd.none
  )



------------
-- UPDATE --
------------

type Msg
  = ToolUpdate Tool
  | MouseDown MouseState
  | MouseUp MouseState
  | MouseUpOutsideElm
  | MouseMove MouseState

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  ( update_ msg model, Cmd.none )

update_ : Msg -> Model -> Model
update_ msg model =
  case msg of
    ToolUpdate tool ->
      { model | tool = tool }

    MouseDown mouse ->
      let
        target =
          ( floor mouse.x // model.zoom
          , floor mouse.y // model.zoom
          )
        grid =
          case model.tool of
            Drawer element ->
              model.grid
                |> Dict.insert target element
            Eraser ->
              model.grid
                |> Dict.remove target
      in
        { model | grid = grid }

    MouseUp mouse ->
      model

    MouseUpOutsideElm ->
      model

    MouseMove mouse ->
      model



----------
-- VIEW --
----------

canvasWidth = 640
canvasHeight = 480

fillBackGound =
  shapes [ fill color.background ] [ rect (0, 0) canvasWidth canvasHeight ]

renderWire ( x, y ) =
  shapes [ fill color.wireActive ]
    [ rect ( toFloat x * 8.0, toFloat y * 8.0 ) 8.0 8.0 ]

view : Model -> Html Msg
view model =
  div [ Attr.class "contents" ]
    [ palletView model.tool
    , blueprintView model
    ]

-- PALLET --

palletView : Tool -> Html Msg
palletView current =
  div []
    [ toolOption "Eraser" Eraser current
    , toolOption "Wire" (Drawer Wire) current
    , toolOption "Bridge" (Drawer Bridge) current
    , toolOption "Button" (Drawer Button) current
    , toolOption "InputPin" (Drawer Input) current
    , toolOption "AndGate" (Drawer And) current
    , toolOption "NorGate" (Drawer Nor) current
    ]

toolOption : String -> Tool -> Tool -> Html Msg
toolOption name tool current =
  div []
    [ input
      [ Attr.type_ "radio"
      , Attr.id name
      , Attr.checked (tool == current)
      , onClick <| ToolUpdate tool
      ] []
    , label [ Attr.for name ] [ text name ]
    ]

-- BLUEPRINT --

blueprintView : Model -> Html Msg
blueprintView model =
  Canvas.toHtml (canvasWidth, canvasHeight)
    [ Attr.style "display" "block"
    , Attr.style "border" "1px solid red"
    , onMouseUp MouseUp  -- holdしているかどうかで移動か
    , onMouseDown MouseDown
    , onMouseMove MouseMove
    ]
    (fillBackGound :: (model.grid |> Dict.toList |> List.map (renderGrid model.zoom)))

renderGrid : Int -> ( Coords, Element ) -> Renderable
renderGrid zoom ( ( x, y ), element ) =
  let
    color_ =
      case element of
        Wire -> color.wireInactive
        Bridge -> color.bridgeInactive
        Button -> color.buttonInactive
        Input -> color.inputInactive
        And -> color.andInactive
        Nor -> color.norInactive
  in
    shapes [ fill color_ ]
      [ rect ( x * zoom |> toFloat, y * zoom |> toFloat ) (toFloat zoom) (toFloat zoom) ]


-------------------
-- SUBSCRIPTIONS --
-------------------

port mouseUp : ( () -> msg ) -> Sub msg

subscriptions : Model -> Sub Msg
subscriptions _ =
    mouseUp (\_ -> MouseUpOutsideElm)



-----------
-- MOUSE --
-----------

type alias MouseState =
  { x : Float
  , y : Float
  , dx : Float
  , dy : Float
  }

mouseDecoder : Json.Decoder MouseState
mouseDecoder =
  Json.map4
    MouseState
    (Json.field "offsetX" Json.float)
    (Json.field "offsetY" Json.float)
    (Json.field "movementX" Json.float)
    (Json.field "movementY" Json.float)

onMouseDown : (MouseState -> msg) -> Attribute msg
onMouseDown msgMapper =
  mouseDecoder
    |> Json.map msgMapper
    |> Html.Events.on "mousedown"

onMouseUp : (MouseState -> msg) -> Attribute msg
onMouseUp msgMapper =
  mouseDecoder
    |> Json.map msgMapper
    |> Html.Events.on "mouseup"

onMouseMove : (MouseState -> msg) -> Attribute msg
onMouseMove msgMapper =
  mouseDecoder
    |> Json.map msgMapper
    |> Html.Events.on "mousemove"



-----------
-- COLOR --
-----------

color =
  { background     = Color.rgb255   0   0   0
  , wireActive     = Color.rgb255   0 255 255
  , wireInactive   = Color.rgb255  64 128 128
  , bridgeActive   = Color.rgb255  64 192 192
  , bridgeInactive = Color.rgb255  32  64  64
  , buttonActive   = Color.rgb255 255 128  32
  , buttonInactive = Color.rgb255 192  96  32
  , inputActive    = Color.rgb255 237  28  36
  , inputInactive  = Color.rgb255 128  28  36
  , andActive      = Color.rgb255 255 192 255
  , andInactive    = Color.rgb255 160 128 160
  , norActive      = Color.rgb255 128 196 128
  , norInactive    = Color.rgb255  96 128  96
  }
