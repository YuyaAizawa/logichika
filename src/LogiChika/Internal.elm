module LogiChika.Internal exposing
  ( fillBackGound
  , renderGrid
  )

import Canvas exposing (Renderable, shapes, rect)
import Canvas.Settings exposing (fill)
import Color exposing (Color)

import LogiChika exposing (Coords, color)


fillBackGound : Int -> Int -> Renderable
fillBackGound width height =
  shapes [ fill color.background ]
    [ rect (0, 0) (toFloat width) (toFloat height) ]

renderGrid : Coords -> Int -> Coords -> Color -> Renderable
renderGrid ( offsetX, offsetY ) zoom ( x, y ) color_ =
  shapes [ fill color_ ]
    [ rect ( (x - offsetX) * zoom |> toFloat, (y - offsetY) * zoom |> toFloat ) (toFloat zoom) (toFloat zoom) ]
