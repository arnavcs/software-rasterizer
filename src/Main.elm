module Main exposing (main)

import Array
import Browser
import Canvas
import Canvas.Settings
import Canvas.Settings.Advanced
import Color
import Debug
import Html
import Html.Attributes
import Html.Events
import Html.Lazy

-----------
-- types --
-----------

type alias Screen = Array.Array (Array.Array Color.Color) -- first index is x coordinate, second index is y coordinate; canvas origin is bottom left

type alias Pixel = (Int, Int)

type alias Point2 = (Float, Float)
type alias Point3 = (Float, Float, Float)

type alias ViewportOpts =
  { width : Float
  , height : Float
  , distance : Float
  }

type alias CanvasOpts =
  { width : Int
  , height : Int
  }

type alias RenderOpts =
  { canvas : CanvasOpts
  , viewport : ViewportOpts
  }

type alias Model = (RenderOpts, RenderOpts)

type Msg = Render
         | CanvasWidthChange  String
         | CanvasHeightChange String

---------------
-- ivm model --
---------------

main : Program () Model Msg
main =
  Browser.sandbox { init = init, view = view, update = update }

init : Model
init =
  let
    opts = { canvas = { width = 300, height = 300 }, viewport = { width = 100, height = 100, distance = 50 } }
  in
    (opts, opts)

view : Model -> Html.Html Msg
view (currRenderOpts, {canvas, viewport}) =
  Html.div
    []
    [ Html.text "Render width: "
    , Html.input [ Html.Attributes.type_ "number"
                 , Html.Attributes.value (String.fromInt canvas.width)
                 , Html.Events.onInput CanvasWidthChange ] []
    , Html.text "Render height: "
    , Html.input [ Html.Attributes.type_ "number"
                 , Html.Attributes.value (String.fromInt canvas.height)
                 , Html.Events.onInput CanvasHeightChange ] []
    , Html.br [] []
    , Html.button [ Html.Events.onClick Render ] [ Html.text "Render Scene" ]
    , Html.hr [] []
    , Html.Lazy.lazy renderCanvas currRenderOpts
    ]

update : Msg -> Model -> Model
update msg (currRenderOpts, ({canvas, viewport} as nextRenderOpts)) =
  case msg of
    Render -> (nextRenderOpts, nextRenderOpts)
    CanvasWidthChange  w -> (currRenderOpts, { nextRenderOpts | canvas = { canvas | width  = Maybe.withDefault 0 (String.toInt w) } })
    CanvasHeightChange h -> (currRenderOpts, { nextRenderOpts | canvas = { canvas | height = Maybe.withDefault 0 (String.toInt h) } })

----------------------
-- canvas utilities --
----------------------

renderCanvas : RenderOpts -> Html.Html Msg
renderCanvas renderOpts =
  Canvas.toHtml
    (renderOpts.canvas.width, renderOpts.canvas.height)
    []
    ((renderToScreen renderOpts) |> (screenToRenderables renderOpts))

screenToRenderables : RenderOpts -> Screen -> List Canvas.Renderable
screenToRenderables {canvas} screen =
  (List.concatMap
     (\ (x, column) ->
        List.map
          (\ (y, color) -> renderablePixel (x, y) color canvas.height)
          (Array.toIndexedList column))
     (Array.toIndexedList screen))

renderablePixel : Pixel -> Color.Color -> Int -> Canvas.Renderable
renderablePixel (x, y) color height =
  let
    cx = toFloat x
    cy = toFloat (height - 1 - y)
  in
    Canvas.shapes
      [ Canvas.Settings.Advanced.imageSmoothing False
      , Canvas.Settings.fill color
      , Canvas.Settings.Advanced.shadow { blur = 0, color = (Color.rgba 0 0 0 0), offset = (0, 0) } ]
      [ Canvas.rect (cx, cy) 1 1 ]

---------------
-- utilities --
---------------

listAt : Int -> List a -> Maybe a
listAt i vs =
  (List.head (List.drop i vs))

zip : List a -> List b -> List (a, b)
zip la lb =
  List.map2 (\ a b -> (a, b)) la lb

zip3 : List a -> List b -> List c -> List (a, b, c)
zip3 la lb lc =
  List.map3 (\ a b c -> (a, b, c)) la lb lc

-- returns the d value of the tuples (i, d) linearly interpolated with independant i0 <= i <= i1
linearFloatInterpolate : (Int, Float) -> (Int, Float) -> List Float
linearFloatInterpolate (i0, d0) (i1, d1) =
  let
    if0 = toFloat i0
    if1 = toFloat i1
  in
    List.map
      (\ x -> ((if1 - x) * d0 + (x - if0) * d1) / (if1 - if0))
      (List.map toFloat <| List.range i0 i1)

linearIntInterpolate : (Int, Int) -> (Int, Int) -> List Float
linearIntInterpolate (i0, d0) (i1, d1) =
  linearFloatInterpolate (i0, toFloat d0) (i1, toFloat d1)

-- returns the d value of the tuples (i, d) linearly interpolated with independant i0 <= i <= i1
linearColourInterpolate : (Int, Color.Color) -> (Int, Color.Color) -> List Color.Color
linearColourInterpolate (i0, c0) (i1, c1) =
  let
    rgba0 = Color.toRgba c0
    rgba1 = Color.toRgba c1
  in
    List.map4
      Color.rgba
      (linearFloatInterpolate (i0, rgba0.red) (i1, rgba1.red))
      (linearFloatInterpolate (i0, rgba0.green) (i1, rgba1.green))
      (linearFloatInterpolate (i0, rgba0.blue) (i1, rgba1.blue))
      (linearFloatInterpolate (i0, rgba0.alpha) (i1, rgba1.alpha))

linearMixedInterpolate : (Int, Int, Color.Color) -> (Int, Int, Color.Color) -> List (Float, Color.Color)
linearMixedInterpolate (i0, f0, c0) (i1, f1, c1) =
  List.map2
    (\ a b -> (a, b))
    (linearIntInterpolate (i0, f0) (i1, f1))
    (linearColourInterpolate (i0, c0) (i1, c1))

------------------
-- 2D rendering --
------------------

renderToScreen : RenderOpts -> Screen
renderToScreen ({ canvas } as renderOpts) =
  let
    s = Array.repeat canvas.width (Array.repeat canvas.height Color.grey)

    a = projectVertex (-50, -50, 100) renderOpts
    b = projectVertex ( 50, -50, 100) renderOpts
    c = projectVertex ( 50,  50, 100) renderOpts
    d = projectVertex (-50,  50, 100) renderOpts
    e = projectVertex (-50, -50, 200) renderOpts
    f = projectVertex ( 50, -50, 200) renderOpts
    g = projectVertex ( 50,  50, 200) renderOpts
    h = projectVertex (-50,  50, 200) renderOpts
  in
    s |> drawFilledTriangle (4, 5) (150, 100) (200, 67) Color.green
      |> drawWireframeTriangle (4, 5) (150, 100) (200, 67) Color.red
      |> drawShadedTriangle ((100, 100), Color.red) ((150, 100), Color.green) ((100, 150), Color.blue)
      |> drawLine a b Color.red
      |> drawLine b c Color.red
      |> drawLine c d Color.red
      |> drawLine d a Color.red
      |> drawLine a e Color.green
      |> drawLine b f Color.green
      |> drawLine c g Color.green
      |> drawLine d h Color.green
      |> drawLine e f Color.blue
      |> drawLine f g Color.blue
      |> drawLine g h Color.blue
      |> drawLine h e Color.blue

putPixel : Pixel -> Color.Color -> Screen -> Screen
putPixel (x, y) color screen =
  let
    column = Maybe.withDefault Array.empty (Array.get x screen)
  in
    Array.set x (Array.set y color column) screen

drawLine : Pixel -> Pixel -> Color.Color -> Screen -> Screen
drawLine (x0, y0) (x1, y1) color screen =
  if (abs (x1 - x0)) > (abs (y1 - y0)) then
    List.foldl
      (\ (x, y) s -> putPixel (x, round y) color s)
      screen
      (if x0 < x1 then
         (List.map2
           (\ a b -> (a, b))
           (List.range x0 x1)
           (linearIntInterpolate (x0, y0) (x1, y1)))
       else
         (List.map2
           (\ a b -> (a, b))
           (List.range x1 x0)
           (linearIntInterpolate (x1, y1) (x0, y0))))
  else
    List.foldl
      (\ (y, x) s -> putPixel (round x, y) color s)
      screen
      (if y0 < y1 then
         (List.map2
           (\ a b -> (a, b))
           (List.range y0 y1)
           (linearIntInterpolate (y0, x0) (y1, x1)))
       else
         (List.map2
           (\ a b -> (a, b))
           (List.range y1 y0)
           (linearIntInterpolate (y1, x1) (y0, x0))))

drawWireframeTriangle : Pixel -> Pixel -> Pixel -> Color.Color -> Screen -> Screen
drawWireframeTriangle p0 p1 p2 color s =
  s |> (drawLine p0 p1 color) |> (drawLine p1 p2 color) |> (drawLine p2 p0 color)

drawFilledTriangle : Pixel -> Pixel -> Pixel -> Color.Color -> Screen -> Screen
drawFilledTriangle ((x0, y0) as p0) ((x1, y1) as p1) ((x2, y2) as p2) color screen =
  if y0 > y1 then
    drawFilledTriangle p1 p0 p2 color screen
  else if y1 > y2 then
    drawFilledTriangle p0 p2 p1 color screen
  else
    let
      x02  = linearIntInterpolate (y0, x0) (y2, x2)
      x01  = linearIntInterpolate (y0, x0) (y1, x1)
      x12  = linearIntInterpolate (y1, x1) (y2, x2)
      x012 = List.append x01 (Maybe.withDefault [] (List.tail x12))

      m = ((y0 + y2) // 2) - y0
      x02atm  = Maybe.withDefault 0 (listAt m x02)
      x012atm = Maybe.withDefault 0 (listAt m x012)

      (left, right) =
        if x02atm < x012atm then (x02, x012) else (x012, x02)
    in
      List.foldl
        (\ (y, xl, xr) scr ->
           List.foldl
             (\ x s -> putPixel (x, y) color s)
             scr
             (List.range (round xl) (round xr)))
        screen
        (zip3 (List.range y0 y2) left right)

drawShadedTriangle : (Pixel, Color.Color) -> (Pixel, Color.Color) -> (Pixel, Color.Color) -> Screen -> Screen
drawShadedTriangle (((x0, y0), c0) as v0) (((x1, y1), c1) as v1) (((x2, y2), c2) as v2) screen =
  if y0 > y1 then
    drawShadedTriangle v1 v0 v2 screen
  else if y1 > y2 then
    drawShadedTriangle v0 v2 v1 screen
  else
    let
      xc02  = linearMixedInterpolate (y0, x0, c0) (y2, x2, c2)
      xc01  = linearMixedInterpolate (y0, x0, c0) (y1, x1, c1)
      xc12  = linearMixedInterpolate (y1, x1, c1) (y2, x2, c2)
      xc012 = List.append xc01 (Maybe.withDefault [] (List.tail xc12))

      m = ((y0 + y2) // 2) - y0
      x02atm  = (listAt m xc02)  |> (Maybe.map Tuple.first) |> (Maybe.withDefault 0)
      x012atm = (listAt m xc012) |> (Maybe.map Tuple.first) |> (Maybe.withDefault 0)

      (left, right) =
        if x02atm < x012atm then (xc02, xc012) else (xc012, xc02)
    in
      List.foldl
        (\ (y, (xfl, cl), (xfr, cr)) scr ->
           let
             xl = round xfl
             xr = round xfr
           in
             List.foldl
               (\ (x, c) s -> putPixel (x, y) c s)
               scr
               (zip (List.range xl xr)
                    (linearColourInterpolate (xl, cl) (xr, cr))))
        screen
        (zip3 (List.range y0 y2) left right)

------------------
-- 3D rendering --
------------------

viewportToScreen : Point2 -> RenderOpts -> Pixel
viewportToScreen (x, y) {canvas, viewport} =
  ( round ((x / viewport.width  + 0.5) * (toFloat canvas.width ))
  , round ((y / viewport.height + 0.5) * (toFloat canvas.height))
  )

projectVertex : Point3 -> RenderOpts -> Pixel
projectVertex (x, y, z) ({canvas, viewport} as renderOpts) =
  viewportToScreen
    (x * viewport.distance / z, y * viewport.distance / z)
    renderOpts

