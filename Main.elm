-- Show an analog clock for your time zone.
--
-- Dependencies:
--   elm install elm/svg
--   elm install elm/time
--
-- For a simpler version, check out:
--   https://elm-lang.org/examples/time
--

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Task
import Time



-- MAIN


main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }



-- MODEL


type alias Model =
  { zone : Time.Zone
  , time : Time.Posix
  }


init : () -> (Model, Cmd Msg)
init _ =
  ( Model Time.utc (Time.millisToPosix 0)
  , Cmd.batch
      [ Task.perform AdjustTimeZone Time.here
      , Task.perform Tick Time.now
      ]
  )



-- UPDATE


type Msg
  = Tick Time.Posix
  | AdjustTimeZone Time.Zone



update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick newTime ->
      ( { model | time = newTime }
      , Cmd.none
      )

    AdjustTimeZone newZone ->
      ( { model | zone = newZone }
      , Cmd.none
      )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every 10 Tick



-- VIEW


view : Model -> Html Msg
view model =
  let
    hour   = toFloat (Time.toHour   model.zone model.time)
    minute = toFloat (Time.toMinute model.zone model.time)
    second = toFloat (Time.toSecond model.zone model.time)
    millis = toFloat (Time.toMillis model.zone model.time)
  in
  svg
    [ viewBox "0 0 400 400"
    , Svg.Attributes.width "400"
    , Svg.Attributes.height "400"
    ]
    [ circle [ cx "200", cy "200", r "150", fill "yellow" ] []
    , circle [ cx "200", cy "200", r "120", fill "#000" ] []
    , viewHandSeconds 3 90 ((second/60) + (millis / 60000))
    , viewHandMinutes 6 90 ((minute/60) + (second / 3600))
    , viewHandHours 6 60 ((hour/12) + (minute / 720) + (second / 3600))
    , drawCircles
    , drawNumbers
    ]
    
drawNumbers =
  svg
    [ viewBox "0 0 400 400"
    , Svg.Attributes.width "400"
    , Svg.Attributes.height "400"
    ]
    [ text_ [x "193", y "70"][Svg.text "12"]
    , text_ [x "330", y "207"][Svg.text "3"]
    , text_ [x "195", y "340"][Svg.text "6"]
    , text_ [x "60", y "207"][Svg.text "9"]
    ]

    
drawCircles =
  svg
    [ viewBox "0 0 400 400"
    , Svg.Attributes.width "400"
    , Svg.Attributes.height "400"
    ]
    [ circle [ cx "200", cy "65", r "15", fill "red" ] []
    , circle [ cx "200", cy "335", r "15", fill "red" ] []
    , circle [ cx "65", cy "200", r "15", fill "red" ] []
    , circle [ cx "335", cy "200", r "15", fill "red" ] []
    ]

viewHandHours : Int -> Float -> Float -> Svg msg
viewHandHours width length turns =
  let
    t = 2 * pi * (turns - 0.25)
    x = 200 + length * cos t
    y = 200 + length * sin t
  in
  line
    [ x1 "200"
    , y1 "200"
    , x2 (String.fromFloat x)
    , y2 (String.fromFloat y)
    , stroke "red"
    , strokeWidth (String.fromInt width)
    , strokeLinecap "round"
    ]
    []
    
viewHandMinutes : Int -> Float -> Float -> Svg msg
viewHandMinutes width length turns =
  let
    t = 2 * pi * (turns - 0.25)
    x = 200 + length * cos t
    y = 200 + length * sin t
  in
  line
    [ x1 "200"
    , y1 "200"
    , x2 (String.fromFloat x)
    , y2 (String.fromFloat y)
    , stroke "white"
    , strokeWidth (String.fromInt width)
    , strokeLinecap "round"
    ]
    []
    
viewHandSeconds : Int -> Float -> Float -> Svg msg
viewHandSeconds width length turns =
  let
    t = 2 * pi * (turns - 0.25)
    x = 200 + length * cos t
    y = 200 + length * sin t
  in
  line
    [ x1 "200"
    , y1 "200"
    , x2 (String.fromFloat x)
    , y2 (String.fromFloat y)
    , stroke "blue"
    , strokeWidth (String.fromInt width)
    , strokeLinecap "round"
    ]
    []