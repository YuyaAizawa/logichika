port module Main exposing (main)

import Browser
import Canvas exposing (Point, Renderable, shapes, rect)
import Canvas.Settings exposing (fill)
import Color
import Dict exposing (Dict)
import Html exposing (Attribute, Html, div, button, input, textarea, label, text)
import Html.Attributes as Attrs
import Html.Events as Events
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

type alias Model =
  { grid : Blueprint
  , tool : Tool
  , zoom : Int
  , text : String
  }

type alias Blueprint = Dict Coords Element

type alias Coords = ( Int, Int )

type Element
  = Wire
  | Bridge
  | Button
  | Input
  | And
  | Nor

type Tool
  = Drawer Element
  | Eraser

init : () -> ( Model, Cmd msg )
init _ =
  (
    { grid = Dict.empty
    , tool = Drawer Wire
    , zoom = 8
    , text = ""
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
  | UpdateText String
  | Serialize
  | Deserialize

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

    UpdateText text_ ->
      { model | text = text_ }

    Serialize ->
      { model | text = serialize model.grid }

    Deserialize ->
      { model | grid = deserialize model.text }



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
  div [ Attrs.class "contents" ]
    [ palletView model.tool
    , blueprintView model
    , serializeView model.text
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
      [ Attrs.type_ "radio"
      , Attrs.id name
      , Attrs.checked (tool == current)
      , Events.onClick <| ToolUpdate tool
      ] []
    , label [ Attrs.for name ] [ text name ]
    ]

-- BLUEPRINT --

blueprintView : Model -> Html Msg
blueprintView model =
  Canvas.toHtml (canvasWidth, canvasHeight)
    [ Attrs.style "display" "block"
    , Attrs.style "border" "1px solid red"
    , onMouseUp MouseUp
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

-- SAVE & LOAD --

serializeView : String -> Html Msg
serializeView text_ =
  div []
    [ textarea
      [ Events.onInput UpdateText
      , Attrs.value text_
      ] []
    , button
      [ Events.onClick Serialize ]
      [ text "Serialize" ]
    , button
      [ Events.onClick Deserialize ]
      [ text "Deserialize" ]
    ]



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
    |> Events.on "mousedown"

onMouseUp : (MouseState -> msg) -> Attribute msg
onMouseUp msgMapper =
  mouseDecoder
    |> Json.map msgMapper
    |> Events.on "mouseup"

onMouseMove : (MouseState -> msg) -> Attribute msg
onMouseMove msgMapper =
  mouseDecoder
    |> Json.map msgMapper
    |> Events.on "mousemove"



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



---------------
-- SERIALIZE --
---------------

serialize : Blueprint -> String
serialize blueprint =
  let
    coordsList = Dict.keys blueprint

    maxLine =
      coordsList
        |> List.map (\( _, y ) -> y)
        |> List.maximum
        |> Maybe.withDefault 0

    maxColumn =
      coordsList
        |> List.map (\( x, y ) -> x)
        |> List.maximum
        |> Maybe.withDefault 0
  in
    List.range 0 maxLine
      |> List.map(\y ->
        List.range 0 maxColumn
          |> List.map(\x ->
            blueprint
              |> Dict.get ( x, y )
              |> toChar
            )
          |> String.fromList
      )
      |> String.join "\n"

deserialize : String -> Blueprint
deserialize src =
  src
    |> String.lines
    |> List.indexedMap (\line string ->
      string
        |> String.toList
        |> List.indexedMap (\column char ->
          ( ( column, line ), fromChar char )
        )
        |> List.filterMap (\( coords, maybeChar ) ->
          maybeChar
            |> Maybe.map(\char -> ( coords, char ))
        )
    )
    |> List.concat
    |> Dict.fromList

toChar : Maybe Element -> Char
toChar element =
  case element of
    Nothing      -> ' '
    Just Wire    -> 'w'
    Just Bridge  -> 'c'
    Just Button  -> 'b'
    Just Input   -> 'p'
    Just And     -> 'a'
    Just Nor     -> 'n'

fromChar : Char -> Maybe Element
fromChar char =
  case char of
    'w' -> Just Wire
    'c' -> Just Bridge
    'b' -> Just Button
    'p' -> Just Input
    'a' -> Just And
    'n' -> Just Nor
    _   -> Nothing
