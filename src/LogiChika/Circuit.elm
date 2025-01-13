module LogiChika.Circuit exposing
  ( Circuit
  , Status
  , compile
  , tick
  , push
  , render
  )

import Array exposing (Array)
import Canvas exposing (Renderable)
import Dict exposing (Dict)
import Set exposing (Set)

import LogiChika exposing (Coords, color)
import LogiChika.Blueprint as Blueprint exposing (Blueprint, Element)
import LogiChika.Internal exposing (..)


buttonRemaining = 5

type Circuit = Circuit
  { logic : Logic
  , status : Status
  }

type alias Logic =
  { gates : Array Gate
  , buttons : Array Button
  , wires : Array Wire
  , inputPins : Array InputPin
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

type alias Element specific =
  { specific
  | region : List Coords
  }
-- TODO: optimizarion point, dividing into rectangles for efficient drawing

type alias Gate =
  Element
  { kind : GateKind
  , inputPins : List InputPinIdx
  }

type GateKind
  = And
  | Nor

type alias Button =
  Element
  {}

type alias Wire =
  Element
  { crosses : List Coords
  , connectedGates : List GateIdx
  , connectedButtons : List ButtonIdx
  }

type alias InputPin =
  Element
  { connectedWires : List WireIdx }

type alias GateIdx = Int
type alias ButtonIdx = Int
type alias WireIdx = Int
type alias InputPinIdx = Int



-------------
-- COMPILE --
-------------

compile : Blueprint -> Circuit
compile blueprint =
  let
    regions : List ( Blueprint.Element, Region )
    regions = splitIntoRegions blueprint

    indexed : ( IndexedRegions, Set Coords )
    indexed = indexRegeions blueprint regions

    logic : Logic
    logic = connectRegions indexed

    status = initStatus
  in
    Circuit { logic = logic, status = status }

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

splitIntoRegions : Blueprint -> List ( Blueprint.Element, Region )
splitIntoRegions blueprint =
  let
    elmentsList = Blueprint.elements blueprint
    elementsDict = Dict.fromList elmentsList
  in
    splitIntoRegionsHelp elmentsList Set.empty [] elementsDict

splitIntoRegionsHelp : List ( Coords, Blueprint.Element ) -> Set Coords -> List ( Blueprint.Element, Region ) -> Dict Coords Blueprint.Element -> List ( Blueprint.Element, Region )
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

getRegionDfs : Blueprint.Element -> List Coords -> List Coords -> List Coords -> Dict Coords Blueprint.Element -> Region
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

indexRegeions : Blueprint -> List ( Blueprint.Element, Region ) -> ( IndexedRegions, Set Coords )
indexRegeions blueprint regions =
  let
    tmpWires : Array Region
    tmpWires =
      regions
        |> List.filterMap (\( kind, region ) ->
          if kind == Blueprint.Wire then
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
      case blueprint |> Blueprint.get start of
        Just Blueprint.Wire ->
          ( trail, tmpWireIdx |> Dict.get start )

        Just Blueprint.Cross ->
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
            Blueprint.Button ->
              { acc | buttonRegions = region :: acc.buttonRegions }

            Blueprint.Input ->
              { acc | inputPinRegions = region :: acc.inputPinRegions }

            Blueprint.And ->
              { acc | gateRegions = (region |> extendsToGateRegion And) :: acc.gateRegions }

            Blueprint.Nor ->
              { acc | gateRegions = (region |> extendsToGateRegion Nor) :: acc.gateRegions }

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
          if kind == Blueprint.Cross then
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

connectRegions : ( IndexedRegions, Set Coords ) -> Logic
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
    Logic gates buttons wires inputPins allCrosses coordsToButton

initStatus : Status
initStatus =
  { active =
    { gates = Set.empty
    , buttons = Set.empty
    , wires = Set.empty
    , inputPins = Set.empty
    }
  , buttonTicks = Dict.empty
  }

insertAll : Set comparable -> List comparable -> Set comparable
insertAll =
  List.foldl Set.insert



----------
-- TICK --
----------

tick : Circuit -> Circuit
tick (Circuit ({ logic, status } as this)) =
  let
    gates =
      logic.gates
        |> Array.toIndexedList
        |> List.filterMap (\( gateIdx, gate ) ->
          case gate.kind of
            And ->
              if gate.inputPins |> List.all (\idx -> Set.member idx status.active.inputPins)
              then Just gateIdx
              else Nothing

            Nor ->
              if gate.inputPins |> List.any (\idx -> Set.member idx status.active.inputPins)
              then Nothing
              else Just gateIdx
        )
        |> Set.fromList

    buttons =
      status.buttonTicks
        |> Dict.keys
        |> Set.fromList

    wires =
      logic.wires
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
      logic.inputPins
        |> Array.toIndexedList
        |> List.filterMap (\( inputPinIdx, { connectedWires } ) ->
          if connectedWires |> List.any (\idx -> Set.member idx wires)
          then Just inputPinIdx
          else Nothing
        )
        |> Set.fromList

    buttonTicks =
      status.buttonTicks
        |> Dict.toList
        |> List.map (\( idx, remaining ) -> ( idx, remaining - 1))
        |> List.filter (\( _, remaining ) -> remaining > 0)
        |> Dict.fromList

    status_ = Status (ActiveElements gates buttons wires inputPins) buttonTicks
  in
    Circuit { this | status = status_ }



----------
-- PUSH --
----------

push : Coords -> Circuit -> Circuit
push coords (Circuit { logic, status } as this) =
  case logic.coordsToButton |> Dict.get coords of
    Nothing ->
      this

    Just buttonIdx ->
      let
        buttonTicks =
          status.buttonTicks |> Dict.insert buttonIdx buttonRemaining

        activeButtons =
          buttonTicks
            |> Dict.keys
            |> Set.fromList

        active_ =
          status.active

        active =
          { active_ | buttons = activeButtons }

        status_ = Status active buttonTicks
      in
        Circuit { logic = logic, status = status_ }



------------
-- RENDER --
------------

render : Int -> Int -> Int -> Circuit -> List Renderable
render width height zoom (Circuit { logic, status }) =
  let
    renderedActiveCrosses =
      status.active.wires
        |> Set.toList
        |> List.concatMap (\idx ->
          case logic.wires |> Array.get idx of
            Nothing ->
              []

            Just { crosses } ->
              crosses |> List.map (\coords -> renderGrid zoom coords color.crossActive)
        )

    renderedCrosses =
      logic.allCrosses
        |> List.map (\coords -> renderGrid zoom coords color.crossInactive)
        |> (\a -> a ++ renderedActiveCrosses)

    renderElements elementsExtractor statusExtractor colorDecider rendered =
      let
        elements = elementsExtractor logic
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
            ( And, True ) -> color.andActive
            ( And, False ) -> color.andInactive
            ( Nor, True ) -> color.norActive
            ( Nor, False ) -> color.norInactive
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
    fillBackGound width height :: renderedElements
