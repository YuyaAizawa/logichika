module LogiChika.Blueprint exposing
  ( Blueprint
  , Element(..)
  , init
  , size
  , get
  , put
  , remove
  , coordsList
  , elements
  , serialize
  , deserialize
  , render
  )

import Canvas exposing (Renderable, shapes, rect)
import Canvas.Settings exposing (fill)
import Color exposing (Color)
import Dict exposing (Dict)

import LogiChika exposing (Coords, color)
import LogiChika.Internal exposing (..)


type Blueprint = Blueprint
  { elements : Dict Coords Element
  , size : ( Int, Int )
  }

type Element
  = Wire
  | Cross
  | Button
  | Input
  | And
  | Nor

init : Int -> Int -> Blueprint
init w h =
  Blueprint
  { elements = Dict.empty
  , size = ( max w 0, max h 0 )
  }

size : Blueprint -> ( Int, Int )
size (Blueprint record) =
  record.size

get : Coords -> Blueprint -> Maybe Element
get coords (Blueprint record) =
  record.elements |> Dict.get coords

put : Coords -> Element -> Blueprint -> Blueprint
put coords element (Blueprint record as this) =
  this
    |> size
    |> validate coords
    |> Maybe.map (\validCoords ->
      Blueprint { record | elements =
        record.elements |> Dict.insert validCoords element
      }
    )
    |> Maybe.withDefault this

remove : Coords -> Blueprint -> Blueprint
remove coords (Blueprint record) =
  Blueprint { record | elements = record.elements |> Dict.remove coords }

validate : Coords -> ( Int, Int ) -> Maybe Coords
validate coords size_ =
  let
    ( x, y ) = coords
    ( w, h ) = size_
  in
    if 0 <= x && x < w && 0 <= y && y < h then
      Just coords
    else
      Nothing

coordsList : Blueprint -> List Coords
coordsList (Blueprint record) =
  Dict.keys record.elements

elements : Blueprint -> List ( Coords, Element )
elements (Blueprint record) =
  Dict.toList record.elements



---------------
-- SERIALIZE --
---------------

serialize : Blueprint -> String
serialize this =
  let
    ( xs, ys ) =
      this
        |> coordsList
        |> List.unzip

    maxColumn =
      xs
        |> List.maximum
        |> Maybe.withDefault 0

    maxLine =
      ys
        |> List.maximum
        |> Maybe.withDefault 0
  in
    List.range 0 maxLine
      |> List.map(\y ->
        List.range 0 maxColumn
          |> List.map(\x ->
            this
              |> get ( x, y )
              |> toChar
            )
          |> String.fromList
      )
      |> String.join "\n"

deserialize : Int -> Int -> String -> Blueprint
deserialize w h src =
  let
    (Blueprint record) = init w h

    elements_ =
      src
        |> String.lines
        |> List.indexedMap (\line string ->
          string
            |> String.toList
            |> List.indexedMap (\column char ->
              ( ( column, line ), fromChar char )
            )
            |> List.filterMap (\( coords, maybeChar ) ->
              Maybe.map2
              (\validCoords char -> ( validCoords, char ))
              (validate coords ( w, h ))
              maybeChar
            )
        )
        |> List.concat
        |> Dict.fromList
  in
    Blueprint { record | elements = elements_ }

toChar : Maybe Element -> Char
toChar element =
  case element of
    Nothing      -> ' '
    Just Wire    -> 'w'
    Just Cross   -> 'c'
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



------------
-- RENDER --
------------

-- TODO: drawing range

render : Int -> Int -> Int -> Blueprint -> List Renderable
render width height zoom this =
  let
    background =
      fillBackGound width height
    elements_ =
      this
        |> elements
        |> List.map (renderElement zoom)
  in
    background :: elements_

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
