module Main exposing (main)

import Browser
import Canvas exposing (Point, shapes, rect)
import Canvas.Settings exposing (fill)
import Color
import Html exposing (Attribute, Html, div)
import Html.Attributes exposing (class, style)
import Html.Events
import Json.Decode as Json



main =
  Browser.sandbox
    { init = init
    , view = view
    , update = update
    }



-----------
-- MODEL --
-----------

type alias Model =
  { block : Point
  , grabbed : Maybe Point
  }

init : Model
init =
  { block = ( 10, 10 )
  , grabbed = Nothing
  }

blockSize = 20



------------
-- UPDATE --
------------

type Msg
  = MouseDown MouseState
  | MouseUp MouseState
  | MouseMove MouseState

update : Msg -> Model -> Model
update msg model =
  case msg of
    MouseDown mouse ->
      { model | grabbed = intoBlock model.block ( mouse.x, mouse.y ) }

    MouseUp mouse ->
      { model | grabbed = Nothing }

    MouseMove mouse ->
      case model.grabbed of
        Nothing ->
          model
        Just ( x, y ) ->
          { model | block = ( mouse.x - x, mouse.y - y ) }

intoBlock : Point -> Point -> Maybe Point
intoBlock ( blockX, blockY ) ( mouseX, mouseY ) =
  let
    x = mouseX - blockX
    y = mouseY - blockY
  in
    if 0 <= x && x < blockSize && 0 <= y && y < blockSize then
      Just ( x, y )
    else
      Nothing



----------
-- VIEW --
----------

canvasWidth = 640
canvasHeight = 480

fillBackGound =
  shapes [ fill Color.white ] [ rect (0, 0) canvasWidth canvasHeight ]

renderBlock upperLeft =
  shapes [ fill Color.red ]
    [ rect upperLeft blockSize blockSize ]

view : Model -> Html Msg
view model =
  div [ class "contents" ]
    [ Canvas.toHtml (canvasWidth, canvasHeight)
      [ style "display" "block"
      , style "border" "1px solid red"
      , onMouseUp MouseUp
      , onMouseDown MouseDown
      , onMouseMove MouseMove
      ]
      [ fillBackGound
      , renderBlock model.block
      ]
    ]



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
