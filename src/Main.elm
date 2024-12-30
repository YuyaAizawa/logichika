module Main exposing (main)

import Browser
import Html exposing (Html, div, text, button)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)



main =
  Browser.sandbox
    { init = init
    , view = view
    , update = update
    }



-- MODEL --

type alias Model =
  { cnt : Int }

init : Model
init =
  { cnt = 3 }



-- UPDATE --

type Msg
  = Increment
  | Decrement

update : Msg -> Model -> Model
update msg model =
  case msg of
    Increment ->
      { model | cnt = min 5 (model.cnt + 1) }

    Decrement ->
      { model | cnt = max 1 (model.cnt - 1) }



-- VIEW --

view : Model -> Html Msg
view model =
  div [ class "contents" ]
    [ div []
        [ button [ onClick Increment ] [ text "👍"]
        , button [ onClick Decrement ] [ text "👎"]
        ]
    , div []
      [ text <| String.concat <| List.repeat model.cnt "🌟" ]
    ]
