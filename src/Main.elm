port module Main exposing (main)

import Browser
import Canvas exposing (Renderable)
import Color exposing (Color)
import Dict exposing (Dict)
import Html exposing (Attribute, Html)
import Html.Attributes as Attrs
import Html.Events as Events
import Json.Decode as Json
import Set exposing (Set)
import Time

import LogiChika exposing (Coords, color)
import LogiChika.Blueprint as Blueprint exposing (Blueprint, Element)
import LogiChika.Circuit as Circuit exposing (Circuit)


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
  { blueprint : Blueprint
  , tool : Tool
  , offset : Coords
  , zoom : Int
  , text : String
  , circuit : Circuit
  , tab : Tab
  , speed : Speed
  }

type Tool
  = Drawer Blueprint.Element
  | Eraser

type Tab
  = EditorTab
  | SimulatorTab

type Speed
  = Pose
  | Turtle
  | Llama
  | Cheetah

init : () -> ( Model, Cmd msg )
init _ =
  let
    blueprint = Blueprint.init canvasWidth canvasHeight
    circuit = Circuit.compile blueprint
  in
    (
      { blueprint = blueprint
      , tool = Drawer Blueprint.Wire
      , offset = ( 0, 0 )
      , zoom = 4
      , text = ""
      , circuit = circuit
      , tab = EditorTab
      , speed = Pose
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
  | TextUpdated String
  | Serialize
  | Deserialize
  | Tick
  | TabClicked Tab
  | SpeedUpdated Speed
  | ZoomUpdated Int
  | Pan Direction


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
      in
        case model.tab of
          EditorTab ->
            let
              blueprint =
                case model.tool of
                  Drawer element ->
                    model.blueprint
                      |> Blueprint.put target element
                  Eraser ->
                    model.blueprint
                      |> Blueprint.remove target
            in
              { model | blueprint = blueprint }

          SimulatorTab ->
            { model | circuit = model.circuit |> Circuit.push target }

    MouseUp mouse ->
      model

    MouseUpOutsideElm ->
      model

    MouseMove mouse ->
      model

    TextUpdated text_ ->
      { model | text = text_ }

    Serialize ->
      { model | text = Blueprint.serialize model.blueprint }

    Deserialize ->
      { model | blueprint = Blueprint.deserialize canvasWidth canvasWidth model.text }

    Tick ->
      { model | circuit = Circuit.tick model.circuit }

    TabClicked tab ->
      if tab == model.tab then
        model
      else
        case tab of
          EditorTab ->
            { model | tab = tab }

          SimulatorTab ->
            { model
            | circuit = Circuit.compile model.blueprint
            , tab = tab
            , speed = Pose
            }

    SpeedUpdated speed ->
      { model | speed = speed }

    ZoomUpdated zoom ->
      { model | zoom = zoom }
        |> validateOffset

    Pan direction ->
      model |> pan direction

type Direction
  = Up
  | Right
  | Down
  | Left

pan : Direction -> Model -> Model
pan direction model =
  let
    ( dx, dy ) =
      case direction of
        Up    -> (  0, -1 )
        Right -> (  1,  0 )
        Down  -> (  0,  1 )
        Left  -> ( -1,  0 )

    panX = dx * canvasWidth  // model.zoom // 2
    panY = dy * canvasHeight // model.zoom // 2

    ( x, y ) = model.offset

    offset = ( x + panX, y + panY )
  in
    { model | offset = offset }
      |> validateOffset

validateOffset : Model -> Model
validateOffset model =
  let
    maxX = canvasWidth  - (canvasWidth  // model.zoom)
    maxY = canvasHeight - (canvasHeight // model.zoom)
    ( x, y ) = model.offset
    offset = ( x |> clamp 0 maxX, y |> clamp 0 maxY )
  in
    { model | offset = offset }



----------
-- VIEW --
----------

canvasWidth = 640   -- must be dividable by the maximum zoom
canvasHeight = 480  -- must be dividable by the maximum zoom

view : Model -> Html Msg
view model =
  let
    tabView : Tab -> String -> Html Msg
    tabView tab text =
      Html.button
        [ Attrs.class "tab"
        , ariaSelected (model.tab == tab)
        , Events.onClick <| TabClicked tab
        ]
        [ Html.text text ]

    tabs =
      Html.div [ Attrs.class "tabs" ]
        [ tabView EditorTab "Editor"
        , tabView SimulatorTab "Simulator"
        ]

    tabContents =
      case model.tab of
        EditorTab ->
          [ blueprintView model
          , Html.div [ Attrs.class "right-column-container"]
            [ Html.div []
              [ palletView model.tool
              , zoomSelectorView model.zoom
              , serializeView model.text
              ]
            , arrowButtonsView
            ]
          ]

        SimulatorTab ->
          [ simulationView model
          , Html.div [ Attrs.class "right-column-container"]
            [ Html.div []
              [ speedSelectorView model.speed
              , zoomSelectorView model.zoom
              , Html.button [ Events.onClick Tick ] [ Html.text "Tick" ]
              ]
            , arrowButtonsView
            ]
          ]
  in
    Html.main_ []
      [ Html.div [ Attrs.class "tab-container" ]
        [ Html.div [ Attrs.class "tab-nav" ] [ tabs ]
        , Html.div [ Attrs.class "tab-contents" ] tabContents
        ]
      ]

arrowButtonsView : Html Msg
arrowButtonsView =
  Html.div [ Attrs.id "arrow-container" ]
    [ Html.button [ Events.onClick <| Pan Up    ] [ Html.text "↑" ]
    , Html.button [ Events.onClick <| Pan Right ] [ Html.text "→" ]
    , Html.button [ Events.onClick <| Pan Down  ] [ Html.text "↓" ]
    , Html.button [ Events.onClick <| Pan Left  ] [ Html.text "←" ]
    ]

ariaSelected : Bool -> Html.Attribute msg
ariaSelected bool =
    Attrs.attribute "aria-selected" (if bool then "true" else "false")

-- PALLET --

palletView : Tool -> Html Msg
palletView current =
  let
    option name color_ tool =
      Html.button
        [ Attrs.class "option"
        , ariaSelected (tool == current)
        , Events.onClick <| ToolUpdate tool
        ]
        [ Html.span
          [ Attrs.style "color" <| Color.toCssString color_ ]
          [ Html.text "■ " ]
        , Html.text name
        ]
  in
    Html.div [ Attrs.class "pallet selector" ]
      [ option "Eraser" color.background Eraser
      , option "Wire" color.wireInactive (Drawer Blueprint.Wire)
      , option "Cross" color.crossInactive (Drawer Blueprint.Cross)
      , option "Button" color.buttonInactive (Drawer Blueprint.Button)
      , option "InputPin" color.inputInactive (Drawer Blueprint.Input)
      , option "AndGate" color.andInactive (Drawer Blueprint.And)
      , option "NorGate" color.norInactive (Drawer Blueprint.Nor)
      ]

-- BLUEPRINT --

blueprintView : Model -> Html Msg
blueprintView { blueprint, offset, zoom } =
  Canvas.toHtml (canvasWidth, canvasHeight)
    [ Attrs.style "display" "block"
    , onMouseUp MouseUp
    , onMouseDown MouseDown
    , onMouseMove MouseMove
    ]
    <| Blueprint.render canvasWidth canvasHeight offset zoom blueprint

-- ZOOM --

zoomSelectorView : Int -> Html Msg
zoomSelectorView current =
  let
    option name zoom =
      Html.button
        [ Attrs.class "option"
        , ariaSelected (zoom == current)
        , Events.onClick <| ZoomUpdated zoom
        ]
        [ Html.text name ]
  in
    Html.div [ Attrs.class "zoom selector" ]
      [ Html.label [] [ Html.text "Zoom: "]
      , option "x1" 1
      , option "x2" 2
      , option "x4" 4
      , option "x8" 8
      , option "x16" 16
      ]

-- SAVE & LOAD --

serializeView : String -> Html Msg
serializeView text =
  Html.div []
    [ Html.textarea
      [ Events.onInput TextUpdated
      , Attrs.value text
      ] []
    , Html.button
      [ Events.onClick Serialize ]
      [ Html.text "Serialize" ]
    , Html.button
      [ Events.onClick Deserialize ]
      [ Html.text "Deserialize" ]
    ]

-- SIMULATION --

simulationView : Model -> Html Msg
simulationView { offset, zoom, circuit } =
  Canvas.toHtml (canvasWidth, canvasHeight)
    [ Attrs.style "display" "block"
    , onMouseDown MouseDown
    ]
    <| Circuit.render canvasWidth canvasHeight offset zoom circuit

-- SPEED --

speedSelectorView : Speed -> Html Msg
speedSelectorView current =
  let
    option name speed =
      Html.button
        [ Attrs.class "option"
        , ariaSelected (speed == current)
        , Events.onClick <| SpeedUpdated speed
        ]
        [ Html.text name ]
  in
    Html.div [ Attrs.class "speed selector" ]
      [ Html.label [] [ Html.text "Speed: "]
      , option "⛄" Pose
      , option "🐢" Turtle
      , option "🦙" Llama
      , option "🐆" Cheetah
      ]



-------------------
-- SUBSCRIPTIONS --
-------------------

port mouseUp : ( () -> msg ) -> Sub msg

subscriptions : Model -> Sub Msg
subscriptions model =
  let
    mouse = mouseUp (\_ -> MouseUpOutsideElm)
    autoTick =
      case model.speed of
        Pose -> Sub.none
        Turtle -> Time.every 500 (\_ -> Tick)
        Llama -> Time.every 96 (\_ -> Tick)
        Cheetah -> Time.every 16 (\_ -> Tick)
  in
    Sub.batch [ mouse, autoTick ]



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
