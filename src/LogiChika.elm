module LogiChika exposing
  ( Coords
  , color
  )


import Color exposing (Color)


type alias Coords = ( Int, Int )

color =
  { background     = Color.rgb255   0   0   0
  , wireActive     = Color.rgb255   0 255 255
  , wireInactive   = Color.rgb255  64 128 128
  , crossActive    = Color.rgb255  64 192 192
  , crossInactive  = Color.rgb255  32  64  64
  , buttonActive   = Color.rgb255 255 128  32
  , buttonInactive = Color.rgb255 192  96  32
  , inputActive    = Color.rgb255 237  28  36
  , inputInactive  = Color.rgb255 128  28  36
  , andActive      = Color.rgb255 255 192 255
  , andInactive    = Color.rgb255 160 128 160
  , norActive      = Color.rgb255 128 196 128
  , norInactive    = Color.rgb255  96 128  96
  }
