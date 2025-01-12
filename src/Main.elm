port module Main exposing (main)

import Array exposing (Array)
import Browser
import Canvas exposing (Point, Renderable, shapes, rect)
import Canvas.Settings exposing (fill)
import Color exposing (Color)
import Dict exposing (Dict)
import Html exposing (Attribute, Html)
import Html.Attributes as Attrs
import Html.Events as Events
import Json.Decode as Json
import Set exposing (Set)
import Time


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
  , zoom : Int
  , text : String
  , circuit : Circuit
  , status : Status
  , tab : Tab
  , speed : Speed
  }

type alias Blueprint = Dict Coords Element

type alias Coords = ( Int, Int )

type Element
  = Wire
  | Cross
  | Button
  | Input
  | And
  | Nor

type Tool
  = Drawer Element
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
    blueprint = Dict.empty
    circuit = compile blueprint
    status = initStatus circuit
  in
    (
      { blueprint = blueprint
      , tool = Drawer Wire
      , zoom = 4
      , text = ""
      , circuit = circuit
      , status = status
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
                      |> Dict.insert target element
                  Eraser ->
                    model.blueprint
                      |> Dict.remove target
            in
              { model | blueprint = blueprint }

          SimulatorTab ->
            case model.circuit.coordsToButton |> Dict.get target of
              Nothing ->
                model

              Just buttonIdx ->
                let
                  buttonTicks =
                    model.status.buttonTicks |> Dict.insert buttonIdx buttonRemaining

                  activeButtons =
                    buttonTicks
                      |> Dict.keys
                      |> Set.fromList

                  active_ =
                    model.status.active

                  active =
                    { active_ | buttons = activeButtons }
                in
                  { model | status = Status active buttonTicks }

    MouseUp mouse ->
      model

    MouseUpOutsideElm ->
      model

    MouseMove mouse ->
      model

    TextUpdated text_ ->
      { model | text = text_ }

    Serialize ->
      { model | text = serialize model.blueprint }

    Deserialize ->
      { model | blueprint = deserialize model.text }

    Tick ->
      { model | status = updateStatus model.circuit model.status }

    TabClicked tab ->
      if tab == model.tab then
        model
      else
        case tab of
          EditorTab ->
            { model | tab = tab }

          SimulatorTab ->
            let
              circuit = compile model.blueprint
              status = initStatus circuit
            in
              { model
              | circuit = circuit
              , status = status
              , tab = tab
              , speed = Pose
              }

    SpeedUpdated speed ->
      { model | speed = speed }



----------
-- VIEW --
----------

canvasWidth = 640
canvasHeight = 480

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

    tabContents =
      case model.tab of
        EditorTab ->
          [ blueprintView model
          , palletView model.tool
          , serializeView model.text
          ]

        SimulatorTab ->
          [ simulationView model.zoom model.circuit model.status
          , speedView model.speed
          , Html.button [ Events.onClick Tick ] [ Html.text "Tick" ]
          ]
  in
    Html.main_ []
      [ Html.div [ Attrs.class "tab-container" ]
        [ Html.div [ Attrs.class "tab-nav" ]
          [ Html.div [ Attrs.class "tabs" ]
              [ tabView EditorTab "Editor"
              , tabView SimulatorTab "Simulator"
              ]
          ]
        , Html.div [ Attrs.class "tab-contents" ] tabContents
        ]
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
    Html.div [ Attrs.class "pallet" ]
      [ option "Eraser" color.background Eraser
      , option "Wire" color.wireInactive (Drawer Wire)
      , option "Cross" color.crossInactive (Drawer Cross)
      , option "Button" color.buttonInactive (Drawer Button)
      , option "InputPin" color.inputInactive (Drawer Input)
      , option "AndGate" color.andInactive (Drawer And)
      , option "NorGate" color.norInactive (Drawer Nor)
      ]

-- BLUEPRINT --

blueprintView : Model -> Html Msg
blueprintView model =
  Canvas.toHtml (canvasWidth, canvasHeight)
    [ Attrs.style "display" "block"
    , onMouseUp MouseUp
    , onMouseDown MouseDown
    , onMouseMove MouseMove
    ]
    (fillBackGound :: (model.blueprint |> Dict.toList |> List.map (renderElement model.zoom)))

renderElement : Int -> ( Coords, Element ) -> Renderable
renderElement zoom ( coords, element ) =
  let
    color_ =
      case element of
        Wire -> color.wireInactive
        Cross -> color.crossInactive
        Button -> color.buttonInactive
        Input -> color.inputInactive
        And -> color.andInactive
        Nor -> color.norInactive
  in
    renderGrid zoom coords color_

fillBackGound : Renderable
fillBackGound =
  shapes [ fill color.background ] [ rect (0, 0) canvasWidth canvasHeight ]

renderGrid : Int -> Coords -> Color -> Renderable
renderGrid zoom ( x, y ) color_ =
  shapes [ fill color_ ]
    [ rect ( x * zoom |> toFloat, y * zoom |> toFloat ) (toFloat zoom) (toFloat zoom) ]

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

simulationView : Int -> Circuit -> Status -> Html Msg
simulationView zoom circuit status =
  let
    renderedActiveCrosses =
      status.active.wires
        |> Set.toList
        |> List.concatMap (\idx ->
          case circuit.wires |> Array.get idx of
            Nothing ->
              []

            Just { crosses } ->
              crosses |> List.map (\coords -> renderGrid zoom coords color.crossActive)
        )

    renderedCrosses =
      circuit.allCrosses
        |> List.map (\coords -> renderGrid zoom coords color.crossInactive)
        |> (\a -> a ++ renderedActiveCrosses)

    renderElements elementsExtractor statusExtractor colorDecider rendered =
      let
        elements = elementsExtractor circuit
        actives = statusExtractor status.active
      in
        elements
          |> Array.toIndexedList
          |> List.foldl (\( idx, element ) acc ->
            let
              isActive = actives |> Set.member idx
              color_ = colorDecider isActive element
              renderables =
                element.region
                  |> List.map (\coords -> renderGrid zoom coords color_)
            in
              renderables ++ acc
          ) rendered

    renderedElements =
      renderedCrosses
        |> renderElements .gates .gates (\isActive { kind } ->
          case ( kind, isActive ) of
            ( LogicAnd, True ) -> color.andActive
            ( LogicAnd, False ) -> color.andInactive
            ( LogicNor, True ) -> color.norActive
            ( LogicNor, False ) -> color.norInactive
        )
        |> renderElements .buttons .buttons (\isActive _ ->
          if isActive then
            color.buttonActive
          else
            color.buttonInactive
        )
        |> renderElements .wires .wires (\isActive _ ->
          if isActive then
            color.wireActive
          else
            color.wireInactive
        )
        |> renderElements .inputPins .inputPins (\isActive _ ->
          if isActive then
            color.inputActive
          else
            color.inputInactive
        )
  in
    Canvas.toHtml (canvasWidth, canvasHeight)
      [ Attrs.style "display" "block"
      , onMouseDown MouseDown
      ]
      (fillBackGound :: renderedElements)

-- SPEED --

speedView : Speed -> Html Msg
speedView current =
  let
    option name speed =
      Html.button
        [ Attrs.class "option"
        , ariaSelected (speed == current)
        , Events.onClick <| SpeedUpdated speed
        ]
        [ Html.text name ]
  in
    Html.div [ Attrs.class "speed" ]
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



-------------
-- CIRCUIT --
-------------

buttonRemaining = 5

type alias Circuit =
  { gates : Array Gate
  , buttons : Array ButtonGroup
  , wires : Array WireGroup
  , inputPins : Array InputPinGroup
  , allCrosses : List Coords
  , coordsToButton : Dict Coords ButtonIdx
  }

type alias Status =
  { active : ActiveElements
  , buttonTicks : Dict ButtonIdx Int
  }

type alias ActiveElements =
  { gates : Set GateIdx
  , buttons : Set ButtonIdx
  , wires : Set WireIdx
  , inputPins : Set InputPinIdx
  }

type alias CircuitElement specific =
  { specific
  | region : List Coords
  }
-- TODO: optimizarion point, dividing into rectangles for efficient drawing

type alias Gate =
  CircuitElement
  { kind : GateKind
  , inputPins : List InputPinIdx
  }

type GateKind
  = LogicAnd
  | LogicNor

type alias ButtonGroup =
  CircuitElement
  {}

type alias WireGroup =
  CircuitElement
  { crosses : List Coords
  , connectedGates : List GateIdx
  , connectedButtons : List ButtonIdx
  }

type alias InputPinGroup =
  CircuitElement
  { connectedWires : List WireIdx }

type alias GateIdx = Int
type alias ButtonIdx = Int
type alias WireIdx = Int
type alias InputPinIdx = Int

updateStatus : Circuit -> Status -> Status
updateStatus circuit prev =
  let
    gates =
      circuit.gates
        |> Array.toIndexedList
        |> List.filterMap (\( gateIdx, gate ) ->
          case gate.kind of
            LogicAnd ->
              if gate.inputPins |> List.all (\idx -> Set.member idx prev.active.inputPins)
              then Just gateIdx
              else Nothing

            LogicNor ->
              if gate.inputPins |> List.any (\idx -> Set.member idx prev.active.inputPins)
              then Nothing
              else Just gateIdx
        )
        |> Set.fromList

    buttons =
      prev.buttonTicks
        |> Dict.keys
        |> Set.fromList

    wires =
      circuit.wires
        |> Array.toIndexedList
        |> List.filterMap (\( wireIdx, { connectedGates, connectedButtons } ) ->
          let
            gatesResult =
              connectedGates
                |> List.any (\idx -> Set.member idx gates)

            buttonsResult =
              connectedButtons
                |> List.any (\idx -> Set.member idx buttons)
          in
            if gatesResult || buttonsResult
            then Just wireIdx
            else Nothing
        )
        |> Set.fromList

    inputPins =
      circuit.inputPins
        |> Array.toIndexedList
        |> List.filterMap (\( inputPinIdx, { connectedWires } ) ->
          if connectedWires |> List.any (\idx -> Set.member idx wires)
          then Just inputPinIdx
          else Nothing
        )
        |> Set.fromList

    buttonTicks =
      prev.buttonTicks
        |> Dict.toList
        |> List.map (\( idx, remaining ) -> ( idx, remaining - 1))
        |> List.filter (\( _, remaining ) -> remaining > 0)
        |> Dict.fromList
  in
    Status (ActiveElements gates buttons wires inputPins) buttonTicks

initStatus : Circuit -> Status
initStatus circuit =
  { active =
    { gates = Set.empty
    , buttons = Set.empty
    , wires = Set.empty
    , inputPins = Set.empty
    }
  , buttonTicks = Dict.empty
  }



-------------
-- COMPILE --
-------------

compile : Blueprint -> Circuit
compile blueprint =
  let
    regions : List ( Element, Region )
    regions = splitIntoRegions blueprint

    indexed : ( IndexedRegions, Set Coords )
    indexed = indexRegeions blueprint regions

    circuit : Circuit
    circuit = connectRegions indexed
  in
    circuit

-- SPLIT --

type alias RegionBase specific =
  { specific
  | region : List Coords     -- connected areas of the same Element
  , neighbors : List Coords  -- adjacent to the 'region' that have different Element
  , surfaces : List Coords   -- adjacent to the 'neighbors' that 'region' contains
  }
type alias Region = RegionBase {}
type alias GateRegion = RegionBase { kind : GateKind }
type alias WireRegion = RegionBase { crosses : List Coords }

mergeRegion to from =
  { region = to.region ++ from.region
  , neighbors = to.neighbors ++ from.neighbors
  , surfaces = to.surfaces ++ from.surfaces  -- NOTE: allow overlap
  }

extendsToGateRegion : GateKind -> Region -> GateRegion
extendsToGateRegion kind base =
  { region = base.region
  , neighbors = base.neighbors
  , surfaces = base.surfaces
  , kind = kind
  }

extendsToWireRegion : List Coords -> Region -> WireRegion
extendsToWireRegion crosses base =
  { region = base.region
  , neighbors = base.neighbors
  , surfaces = base.surfaces
  , crosses = crosses
  }

getReverseLookup : (a -> List comparable) -> Array a -> Dict comparable Int
getReverseLookup keysExtractor array =
  array
    |> Array.toIndexedList
    |> List.concatMap (\( idx, value ) ->
      value
        |> keysExtractor
        |> List.map (\key -> ( key, idx ))
    )
    |> Dict.fromList

splitIntoRegions : Blueprint -> List ( Element, Region )
splitIntoRegions blueprint =
  splitIntoRegionsHelp (Dict.toList blueprint) Set.empty [] blueprint

splitIntoRegionsHelp : List ( Coords, Element ) -> Set Coords -> List ( Element, Region ) -> Blueprint -> List ( Element, Region )
splitIntoRegionsHelp candidates processeds result blueprint =
  case candidates of
    [] ->
      result

    ( coords, element ) :: rest ->
      if processeds |> Set.member coords then
        splitIntoRegionsHelp rest processeds result blueprint
      else
        let
          region =
            getRegionDfs element [ coords ] [] [] blueprint

          processeds_ =
            region.region |> insertAll processeds

          result_ =
            ( element, region ) :: result
        in
          splitIntoRegionsHelp rest processeds_ result_ blueprint

getRegionDfs : Element -> List Coords -> List Coords -> List Coords -> Blueprint -> Region
getRegionDfs target stack region neighbors blueprint =
  case stack of
    [] ->
      { region = region
      , neighbors = neighbors
      , surfaces = getSurfaces neighbors region
      } |> Debug.log "getRegionDfs"

    hd :: tl ->
      case blueprint |> Dict.get hd of
        Nothing ->
          getRegionDfs target tl region neighbors blueprint

        Just element ->
          let
            ( stack_, region_, neighbors_ ) =
              if element == target then
                ( getNeighbors hd ++ tl
                , hd :: region
                , neighbors
                )
              else
                ( tl
                , region
                , hd :: neighbors
                )

            blueprint_ =
              blueprint |> Dict.remove hd
          in
            getRegionDfs target stack_ region_ neighbors_ blueprint_

getNeighbors : Coords -> List Coords
getNeighbors ( x, y ) =
  [ ( x + 1, y )
  , ( x - 1, y )
  , ( x, y + 1 )
  , ( x, y - 1 )
  ]

getSurfaces : List Coords -> List Coords -> List Coords
getSurfaces neighbors region =
  let
    regionSet =
      Set.fromList region
  in
    neighbors
      |> List.concatMap getNeighbors
      |> List.filter (\coords -> regionSet |> Set.member coords)

-- INDEX --

type alias IndexedRegions =
  { wireRegions : Array WireRegion
  , wireIdxFromCoords : Dict Coords Int
  , buttonRegions : Array Region
  , buttonIdxFromCoords : Dict Coords Int
  , inputPinRegions : Array Region
  , inputPinIdxFromCoords : Dict Coords Int
  , gateRegions : Array GateRegion
  , gateIdxFromCoords : Dict Coords Int
  }

indexRegeions : Blueprint -> List ( Element, Region ) -> ( IndexedRegions, Set Coords )
indexRegeions blueprint regions =
  let
    tmpWires : Array Region
    tmpWires =
      regions
        |> List.filterMap (\( kind, region ) ->
          if kind == Wire then
            Just region
          else
            Nothing
        )
        |> Array.fromList

    tmpWireIdx : Dict Coords Int
    tmpWireIdx =
      tmpWires |> getReverseLookup .surfaces

    trailCross : Coords -> Coords -> List Coords -> ( List Coords, Maybe Int )
    trailCross start direction trail =
      case blueprint |> Dict.get start of
        Just Wire ->
          ( trail, tmpWireIdx |> Dict.get start )

        Just Cross ->
          let
            ( x, y ) = start
            ( dx, dy ) = direction
            next = ( x + dx, y + dy )
            trail_ = start :: trail
          in
            trailCross next direction trail_

        _ ->
          ( trail, Nothing )

    getWireGroupDfs : List Int -> Set Coords -> Set Int -> List Coords -> ( List Int, List Coords )
    getWireGroupDfs stack knownSurfaces idxes crosses =
      case stack of
        [] ->
          ( idxes |> Set.toList, crosses )

        hd :: tl ->
          if idxes |> Set.member hd then
            getWireGroupDfs tl knownSurfaces idxes crosses
          else
            case tmpWires |> Array.get hd of
              Nothing -> ( [], [] )  -- Never happen
              Just { neighbors, surfaces } ->
                let
                  knownSurfaces_ =
                    surfaces |> insertAll knownSurfaces

                  idxes_ =
                    Set.insert hd idxes

                  ( crossLists, maybeWireIdxes ) =
                    neighbors
                      |> List.concatMap (\(( nx, ny ) as n) ->
                        getNeighbors n
                          |> List.filter (\s -> knownSurfaces_ |> Set.member s)
                          |> List.map (\( sx, sy ) -> ( nx - sx, ny - sy ))
                          |> List.map (\d -> trailCross n d [])
                      )
                      |> List.unzip

                  stack_ =
                    (maybeWireIdxes |> List.filterMap (\mid -> mid)) ++ tl

                  crosses_ =
                    (List.concat crossLists) ++ crosses
                in
                  getWireGroupDfs stack_ knownSurfaces_ idxes_ crosses_

    groupWire : Int -> Set Int -> List WireRegion -> Array WireRegion
    groupWire tmpIdx processeds result =
      case tmpWires |> Array.get tmpIdx of
        Nothing ->
          result |> Array.fromList

        Just _ ->
          if processeds |> Set.member tmpIdx then
            groupWire (tmpIdx + 1) processeds result
          else
            let
              ( tmpIdxes, crosses ) =
                getWireGroupDfs [ tmpIdx ] Set.empty Set.empty []

              wirePart =
                tmpIdxes |> List.foldl (\tmpIdx_ acc ->
                  case tmpWires |> Array.get tmpIdx_ of
                    Nothing ->
                      acc

                    Just tmpWire ->
                      mergeRegion tmpWire acc
                ) { region = [], neighbors = [], surfaces = [] }

              processeds_ =
                tmpIdxes |> insertAll processeds

              result_ =
                (wirePart |> extendsToWireRegion crosses) :: result
            in
              groupWire (tmpIdx + 1) processeds_ result_

    wireRegions =
      groupWire 0 Set.empty []

    { buttonRegions, inputPinRegions, gateRegions } =
      regions
        |> List.foldl (\( kind, region ) acc ->
          case kind of
            Button ->
              { acc | buttonRegions = region :: acc.buttonRegions }

            Input ->
              { acc | inputPinRegions = region :: acc.inputPinRegions }

            And ->
              { acc | gateRegions = (region |> extendsToGateRegion LogicAnd) :: acc.gateRegions }

            Nor ->
              { acc | gateRegions = (region |> extendsToGateRegion LogicNor) :: acc.gateRegions }

            _ ->
              acc
        ) { buttonRegions = [], inputPinRegions = [], gateRegions = [] }
        |> (\listStyle ->
          { buttonRegions = Array.fromList listStyle.buttonRegions
          , inputPinRegions = Array.fromList listStyle.inputPinRegions
          , gateRegions = Array.fromList listStyle.gateRegions
          }
        )

    crossCoords =
      regions
        |> List.concatMap (\( kind, { region } ) ->
          if kind == Cross then
            region
          else
            []
        )
        |> Set.fromList
  in
    ( { wireRegions = wireRegions
      , wireIdxFromCoords = wireRegions |> getReverseLookup .surfaces
      , buttonRegions = buttonRegions
      , buttonIdxFromCoords = buttonRegions |> getReverseLookup .surfaces
      , inputPinRegions = inputPinRegions
      , inputPinIdxFromCoords = inputPinRegions |> getReverseLookup .surfaces
      , gateRegions = gateRegions
      , gateIdxFromCoords = gateRegions |> getReverseLookup .surfaces
      }
    , crossCoords
    )

-- CONNECT --

connectRegions : ( IndexedRegions, Set Coords ) -> Circuit
connectRegions
  ( { wireRegions
    , wireIdxFromCoords
    , buttonRegions
    , buttonIdxFromCoords
    , inputPinRegions
    , inputPinIdxFromCoords
    , gateRegions
    , gateIdxFromCoords
    }
  , crossCoords
  ) =
  let
    coordsToIdxes idxes dict =
      idxes |> List.filterMap (\idx -> Dict.get idx dict)

    gates =
      gateRegions |> Array.map (\{ region, neighbors, kind } ->
        let
          inputPins_ =
            coordsToIdxes neighbors inputPinIdxFromCoords
        in
          { region = region, kind = kind, inputPins = inputPins_ }
      )

    buttons =
      buttonRegions |> Array.map (\{ region } -> { region = region } )

    wires =
      wireRegions |> Array.map (\{ region, neighbors, crosses } ->
        let
          connectedGates =
            coordsToIdxes neighbors gateIdxFromCoords

          connectedButtons =
            coordsToIdxes neighbors buttonIdxFromCoords
        in
          { region = region, crosses = crosses, connectedGates = connectedGates, connectedButtons = connectedButtons }
      )

    inputPins =
      inputPinRegions |> Array.map (\{ region, neighbors } ->
        let
          connectedWires =
            coordsToIdxes neighbors wireIdxFromCoords
        in
          { region = region, connectedWires = connectedWires }
      )

    allCrosses = Set.toList crossCoords

    coordsToButton =
      buttonRegions
        |> Array.toIndexedList
        |> List.concatMap (\( idx, { region }) ->
          region |> List.map (\coords -> ( coords, idx ))
        )
        |> Dict.fromList
  in
    Circuit gates buttons wires inputPins allCrosses coordsToButton



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
  , crossActive   = Color.rgb255  64 192 192
  , crossInactive = Color.rgb255  32  64  64
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
    Just Cross  -> 'c'
    Just Button  -> 'b'
    Just Input   -> 'p'
    Just And     -> 'a'
    Just Nor     -> 'n'

fromChar : Char -> Maybe Element
fromChar char =
  case char of
    'w' -> Just Wire
    'c' -> Just Cross
    'b' -> Just Button
    'p' -> Just Input
    'a' -> Just And
    'n' -> Just Nor
    _   -> Nothing



-----------
-- UTILS --
-----------

insertAll : Set comparable -> List comparable -> Set comparable
insertAll =
  List.foldl Set.insert
