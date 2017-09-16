module Client.Rubik

open System
open Fable.Core
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Style
open Messages

open Fable.Import.Browser
open Fable.Core.JsInterop

open Elmish
open Elmish.React

/// Animation
let timerTick dispatch =
    window.setInterval(fun _ -> 
        dispatch (Tick DateTime.Now |> RubikMsg)
    , 100) |> ignore


let subscription _ = Cmd.ofSub timerTick

/// Model

type Face =
  | North
  | East
  | South
  | West
  | Top
  | Bottom

type Direction =
  | Clockwise
  | CounterClockwise

type Turn =
  { face: Face
    direction: Direction }

type Turning =
  { turn: Turn
    started: DateTime
    progress: float } // should run from 0.0 to 1.0 completing the turn

type Model =
  { colouring: Face[]
    turning: Turning option }

let initialColouring =
  [| North
     East
     South
     West
     Top
     Bottom
  |]
  |> Array.collect (Array.create 9)

let init =
  { colouring = initialColouring
    turning = None }

/// Messages - not used yet

// type Message =
//   | Turn of Turn
//   | Tick of DateTime

/// Update

let apply turn (colouring: Face[]) =
  match turn.face with
  | North ->
    [|
        yield! [| 0..8 |]
        yield! [| 20; 18; 11; 12; 24; 14; 23; 22; 26 |]
        yield! [| 36; 19; 37; 21; 42; 43; 40; 25; 44 |]
        yield! [| 28; 30; 27; 29; 34; 33; 31; 32; 35 |]
        yield! [| 47; 45; 38; 39; 51; 41; 50; 49; 53 |]
        yield! [|  9; 46; 10; 48; 15; 16; 13; 52; 17 |]
    |]
    |> Array.map (fun i -> colouring.[i])
  | West ->
    [|
        yield! [| 0..8 |]
        yield! [| 20; 18; 11; 12; 24; 14; 15; 16; 17 |]
        yield! [| 36; 19; 37; 21; 22; 23; 40; 25; 26 |]
        yield! [| 28; 30; 27; 29; 34; 33; 31; 32; 35 |]
        yield! [| 47; 45; 38; 39; 51; 41; 42; 43; 44 |]
        yield! [|  9; 46; 10; 48; 49; 50; 13; 52; 53 |]
    |]
    |> Array.map (fun i -> colouring.[i])
  | South ->
    [|
        yield! [| 11;  9;  2;  3; 15;  5; 14; 13; 17 |]
        yield! [| 27; 10; 28; 12; 33; 34; 31; 16; 35 |]
        yield! [| 18..26 |]
        yield! [| 38; 36; 29; 30; 42; 32; 41; 40; 44 |]
        yield! [|  0; 37;  1; 39;  6;  7;  4; 43;  8 |]
        yield! [| 46; 48; 45; 47; 52; 51; 49; 50; 53 |]
    |]
    |> Array.map (fun i -> colouring.[i])
  | Bottom ->
    [|
        yield! [| 11;  9;  2;  3; 15;  5;  6;  7;  8 |]
        yield! [| 27; 10; 28; 12; 13; 14; 31; 16; 17 |]
        yield! [| 18..26 |]
        yield! [| 38; 36; 29; 30; 42; 32; 33; 34; 35 |]
        yield! [|  0; 37;  1; 39; 40; 41;  4; 43; 44 |]
        yield! [| 46; 48; 45; 47; 52; 51; 49; 50; 53 |]
    |]
    |> Array.map (fun i -> colouring.[i])
  | East ->
    [|
        yield! [| 45;  1; 46;  3; 51; 52; 49;  7; 53 |]
        yield! [| 9..17 |]
        yield! [|  2;  0; 20; 21;  6; 23;  5;  4;  8 |]
        yield! [| 18; 28; 19; 30; 24; 25; 22; 34; 26 |]
        yield! [| 37; 39; 36; 38; 43; 42; 40; 41; 44 |]
        yield! [| 29; 27; 47; 48; 33; 50; 32; 31; 35 |]
    |]
    |> Array.map (fun i -> colouring.[i])
  | Top ->
    [|
        yield! [| 45;  1; 46;  3;  4;  5; 49;  7;  8 |]
        yield! [| 9..17 |]
        yield! [|  2;  0; 20; 21;  6; 23; 24; 25; 26 |]
        yield! [| 18; 28; 19; 30; 31; 32; 22; 34; 35 |]
        yield! [| 37; 39; 36; 38; 43; 42; 40; 41; 44 |]
        yield! [| 29; 27; 47; 48; 33; 50; 51; 52; 53 |]
    |]
    |> Array.map (fun i -> colouring.[i])

// milliseconds it takes to complete a turn
let turnTime = 3000.0

let updateTick t (model: Model) =
  match model.turning with
  | None ->
    model
  | Some turning ->
    let dt = (DateTime.Now - turning.started).TotalMilliseconds
    let p = float dt / turnTime |> min 1.0
    if p < 1.0 then
      let newTurning = { turning with progress = p }
      { model with turning = Some newTurning }
    else
      { colouring = apply turning.turn model.colouring
        turning = None }

let update (msg:RubikMsg) model : Model*Cmd<RubikMsg> =
  match msg with
  | Tick t ->
    updateTick t model, []
  | TurnNorthCW ->
    let turn = { face = North; direction = Clockwise }
    let turning = { turn = turn; started = System.DateTime.Now; progress = 0.0 }
    { model with turning = Some turning }, []
  | TurnNorthCCW ->
    let turn = { face = North; direction = CounterClockwise }
    let turning = { turn = turn; started = System.DateTime.Now; progress = 0.0 }
    { model with turning = Some turning }, []
  | TurnSouthCW ->
    let turn = { face = South; direction = Clockwise }
    let turning = { turn = turn; started = System.DateTime.Now; progress = 0.0 }
    { model with turning = Some turning }, []
  | TurnSouthCCW ->
    let turn = { face = South; direction = CounterClockwise }
    let turning = { turn = turn; started = System.DateTime.Now; progress = 0.0 }
    { model with turning = Some turning }, []
  | TurnEastCW ->
    let turn = { face = East; direction = Clockwise }
    let turning = { turn = turn; started = System.DateTime.Now; progress = 0.0 }
    { model with turning = Some turning }, []
  | TurnEastCCW ->
    let turn = { face = East; direction = CounterClockwise }
    let turning = { turn = turn; started = System.DateTime.Now; progress = 0.0 }
    { model with turning = Some turning }, []
  | TurnWestCW ->
    let turn = { face = West; direction = Clockwise }
    let turning = { turn = turn; started = System.DateTime.Now; progress = 0.0 }
    { model with turning = Some turning }, []
  | TurnWestCCW ->
    let turn = { face = West; direction = CounterClockwise }
    let turning = { turn = turn; started = System.DateTime.Now; progress = 0.0 }
    { model with turning = Some turning }, []
  | TurnTopCW ->
    let turn = { face = Top; direction = Clockwise }
    let turning = { turn = turn; started = System.DateTime.Now; progress = 0.0 }
    { model with turning = Some turning }, []
  | TurnTopCCW ->
    let turn = { face = Top; direction = CounterClockwise }
    let turning = { turn = turn; started = System.DateTime.Now; progress = 0.0 }
    { model with turning = Some turning }, []
  | TurnBottomCW ->
    let turn = { face = Bottom; direction = Clockwise }
    let turning = { turn = turn; started = System.DateTime.Now; progress = 0.0 }
    { model with turning = Some turning }, []
  | TurnBottomCCW ->
    let turn = { face = Bottom; direction = CounterClockwise }
    let turning = { turn = turn; started = System.DateTime.Now; progress = 0.0 }
    { model with turning = Some turning }, []

/// View

/// 3D vectors and matrices
type V3 =
  { x: float
    y: float
    z: float }

let scale c (v: V3) =
  { x = c * v.x
    y = c * v.y
    z = c * v.z }

let add v1 v2 =
  { x = v1.x + v2.x
    y = v1.y + v2.y
    z = v1.z + v2.z }

let dot v1 v2 =
  v1.x*v2.x + v1.y*v2.y + v1.z*v2.z

type M33 =
  { r1: V3
    r2: V3
    r3: V3 }

let mul m v =
  { x = dot m.r1 v 
    y = dot m.r2 v
    z = dot m.r3 v }

let baseM vx vy vz =
  { r1 = { x = vx.x; y = vy.x; z = vz.x}
    r2 = { x = vx.y; y = vy.y; z = vz.y}
    r3 = { x = vx.z; y = vy.z; z = vz.z} }

/// 3D paths
let toFace p1 p2 p3 p4 =
  p1 @ List.tail p2 @ List.tail p3 @ List.tail p4

let pi = System.Math.PI

let rotate v =
  { x = v.z; y = v.x; z = v.y }

let flipX v =
  { x = -v.x; y = v.y; z = v.z }
let flipY v =
  { x = v.x; y = -v.y; z = v.z }
let flipZ v =
  { x = v.x; y = v.y; z = -v.z }

let mirrorXY v =
  { x = v.y; y = v.x; z = v.z }

let mirrorXZ v =
  { x = v.z; y = v.y; z = v.x }

let mirrorYZ v =
  { x = v.x; y = v.z; z = v.y }

let up = id
let down = List.rev
let rot = List.map rotate
let mXY = List.map mirrorXY
let mXZ = List.map mirrorXZ
let mYZ = List.map mirrorYZ
let fXY = List.map (flipY >> flipX)
let fX = List.map flipX
let fY = List.map flipY
let fZ = List.map flipZ

let rec mapEveryOther first f l =
  let mapFirst x =
    if first then f x else x
  match l with
  | [] -> []
  | x::xs -> mapFirst x :: mapEveryOther (not first) f xs

let tanLen v =
   (tan v)*(tan v) + 1.0 |> sqrt

let cd = 0.15

// angle at which the circles live
let cv = asin cd
let cornerLine n =
  let s = 0.5 |> sqrt
  let ev = 1.0 / (sqrt 2.0) |> atan
  let dv = ev - cv
  let v i = cv + (float i)*dv/(float n)
  let l = dv / float n |> tanLen
  [0..n]
  |> List.map (fun i -> { x = s * cos (v i); y = -s * cos (v i); z = -sin (v i) })
  |> mapEveryOther false (scale l)

let edgeLine n =
  let s = 0.5 |> sqrt
  let dv = 2.0 * cv
  let rc = cos cv
  let v i = cv - (float i)*dv/(float n)
  let l = dv / float n |> tanLen
  [0..n]
  |> List.map (fun i -> { x = -s * cos (v i); y = s * cos (v i); z = rc * sin (v i) })
  |> mapEveryOther false (scale l)

let faceLine n =
  let x = sin cv
  let tv = tan cv |> asin
  let dv = pi / 4.0 - tv
  let rc = cos cv
  let v i = tv + (float i)*dv/(float n)
  let l = dv / float n |> tanLen
  [0..n]
  |> List.map (fun i -> { x = -x; y = rc * cos (v i); z = -rc * sin (v i) })
  |> mapEveryOther false (fun v -> { x = v.x; y = l*v.y; z = l*v.z })

let centerLine n =
  let x = sin cv
  let tv = tan cv |> asin
  let dv = 2.0 * tv
  let rc = cos cv
  let v i = tv - (float i)*dv/(float n)
  let l = dv / float n |> tanLen
  [0..n]
  |> List.map (fun i -> { x = -x; y = rc * cos (v i); z = rc * sin (v i) })
  |> mapEveryOther false (fun v -> { x = v.x; y = l*v.y; z = l*v.z })

let corner n =
  toFace
    (up (cornerLine n |> fXY))
    (down (cornerLine n |> rot))
    (down (faceLine n))
    (up (faceLine n |> mXY |> rot))

let center n =
  toFace
    (up (centerLine n))
    (down (centerLine n |> mXZ))
    (down (centerLine n |> fX))
    (up (centerLine n |> mXZ |> fZ))

let edge n =
  toFace
    (up (edgeLine n))
    (down (faceLine n |> mXZ))
    (down (centerLine n))
    (up (faceLine n |> mXZ |> fZ))

let corners corner =
  [ corner 
    corner |> fZ
    corner |> fX
    corner |> fX |> fZ
  ]

let edges edge =
  [ edge
    edge |> fX
    edge |> mXZ
    edge |> fX |> mXZ
  ]

let paths3D =
  [ yield! corners (corner 6)
    yield! edges (edge 6)
    yield! [center 6]
    yield! corners (corner 6) |> List.map rot
    yield! edges (edge 6) |> List.map rot
    yield! [center 6] |> List.map rot
    yield! corners (corner 6) |> List.map (rot >> rot)
    yield! edges (edge 6) |> List.map (rot >> rot)
    yield! [center 6] |> List.map (rot >> rot)
    yield! corners (corner 6 |> fY)
    yield! edges (edge 6 |> fY)
    yield! [center 6] |> List.map fY
    yield! corners (corner 6 |> fY) |> List.map rot
    yield! edges (edge 6 |> fY) |> List.map rot
    yield! [center 6] |> List.map (fY >> rot)
    yield! corners (corner 6 |> fY) |> List.map (rot >> rot)
    yield! edges (edge 6 |> fY) |> List.map (rot >> rot)
    yield! [center 6] |> List.map (fY >> rot >> rot)
  ]
  |> Array.ofList

/// Selection
let edgeLeft = [| 0; 1; 4 |]
let edgeRight = [| 0; 2; 6 |]
let centerA = [| 4; 5; 8 |]
let centerB = [| 6; 7; 8 |]
let moveFace n = Array.map ((+) (9*n))

let indices =
  [ North,
    [| yield! moveFace 1 edgeLeft
       yield! moveFace 1 centerB
       yield! moveFace 2 edgeRight
       yield! moveFace 2 centerA
       yield! moveFace 3 [|0..8|]
       yield! moveFace 4 edgeLeft
       yield! moveFace 4 centerB
       yield! moveFace 5 edgeRight
       yield! moveFace 5 centerA
    |]
    // |> Array.sort
  ; East,
    [| yield! edgeRight
       yield! centerA
       yield! moveFace 2 edgeLeft
       yield! moveFace 2 centerB
       yield! moveFace 3 edgeRight
       yield! moveFace 3 centerA
       yield! moveFace 4 [|0..8|]
       yield! moveFace 5 edgeLeft
       yield! moveFace 5 centerB
    |]
    // |> Array.sort
  ; South,
    [| yield! edgeLeft
       yield! centerB
       yield! moveFace 1 edgeRight
       yield! moveFace 1 centerA
       yield! moveFace 3 edgeLeft
       yield! moveFace 3 centerB
       yield! moveFace 4 edgeRight
       yield! moveFace 4 centerA
       yield! moveFace 5 [|0..8|]
    |]
    // |> Array.sort
  ; West,
    [| yield! moveFace 1 edgeLeft
       yield! moveFace 2 edgeRight
       yield! moveFace 3 [|0..8|]
       yield! moveFace 4 edgeLeft
       yield! moveFace 5 edgeRight
    |]
    // |> Array.sort
  ; Top,
    [| yield! edgeRight
       yield! moveFace 2 edgeLeft
       yield! moveFace 3 edgeRight
       yield! moveFace 4 [|0..8|]
       yield! moveFace 5 edgeLeft
    |]
    // |> Array.sort
  ; Bottom,
    [| yield! edgeLeft
       yield! moveFace 1 edgeRight
       yield! moveFace 3 edgeLeft
       yield! moveFace 4 edgeRight
       yield! moveFace 5 [|0..8|]
    |]
    // |> Array.sort
  ]
  |> Map.ofList

/// 2D rendering
let rec pairs l =
  match l with
  | [] -> []
  | [_] -> failwith "need an even number to get pairs"
  | x::y::rest -> (x,y)::pairs rest

let toQs points =
  match points with
  | [] | [_] | [_;_] -> failwith "Too few points (need at least three)"
  | (x,y)::ps ->
    ps
    |> pairs
    |> List.map (fun ((cx, cy), (px,py)) -> sprintf "Q %f %f %f %f" cx cy px py)
    |> String.concat "\n       "
    |> sprintf "M %f %f\n       %s" x y

let wrapPath style p =
  path (style @ [D p]) [] 

let loop =
  sprintf "%s\n       z"

let toQLoop style points =
  toQs points
  |> loop
  |> wrapPath style

let m =
  { r1 = { x = -0.7071067690849304; y = 0.7071067690849304; z = 0.0}
    r2 = { x = 0.3933604061603546; y = 0.3933604061603546; z = -0.8309844732284546}
    r3 = { x = -0.6000000238418579; y = -0.6000000238418579; z = -0.6000000238418579}
  }
let d = 0.6
let p =
  { x = -d
    y = -d 
    z = -d }

let projLinear v =
  let c = if v.z = 0.0 then 1.0 else 1.0 / v.z
  scale c v

let projPot p v =
  let c = if v.z = 0.0 then 1.0 else 1.0 / (v.z ** p)
  scale c v

let proj = projPot 0.8

let toView = add p >> mul m

let toPlane = toView >> proj

let toScreenCanvas xMin yMin xMax yMax targetWidth v =
  let width = xMax - xMin
  let height = yMax - yMin
  let scale = targetWidth / width
  (scale * (v.x-xMin), scale * (v.y-yMin))

let width = 600.0
let windowRadius = 2.5

let toScreen = toScreenCanvas -windowRadius -windowRadius windowRadius windowRadius width
let t = toPlane >> toScreen |> List.map


let faceColour = function
  | North -> "green"
  | East -> "orange"
  | South -> "purple"
  | West -> "red"
  | Top -> "blue"
  | Bottom -> "yellow"
  
let faceStyle colour =
  [ Fill colour ]
  
let colour c = t >> toQLoop (faceStyle c)

let nearCornerNorth n style =
  let p1 =
    (down (faceLine n |> mXZ |> fZ))
    |> fX
    |> t
  let p2 =
    (up (faceLine n |> mXZ |> fZ))
    |> fX |> mXZ
    |> t
  let x1 = 300.0 + 297.0 * cos (pi / 6.0)
  let y1 = 300.0 - 297.0 * sin (pi / 6.0)
  p1 @ List.tail p2
  |> toQs
  |> fun s -> sprintf "%s\n       L %f %f" s x1 y1
  |> sprintf "%s\n       A 297 297 0 0 1 300 597"
  |> loop
  |> wrapPath style

let nearCornerEast n style =
  let p1 =
    (down (faceLine n |> mXZ |> fZ))
    |> fX |> rot
    |> t
  let p2 =
    (up (faceLine n |> mXZ |> fZ))
    |> fX |> mXZ |> rot
    |> t
  let x1 = 300.0 + 297.0 * cos (5.0 * pi / 6.0)
  let y1 = 300.0 - 297.0 * sin (5.0 * pi / 6.0)
  let x2 = 300.0 + 297.0 * cos (pi / 6.0)
  let y2 = 300.0 - 297.0 * sin (pi / 6.0)
  p1 @ List.tail p2
  |> toQs
  |> fun s -> sprintf "%s\n       L %f %f" s x1 y1
  |> fun s -> sprintf "%s\n       A 297 297 0 0 1 %f %f" s x2 y2
  |> loop
  |> wrapPath style

let nearCornerSouth n style =
  let p1 =
    (down (faceLine n |> mXZ |> fZ))
    |> fX |> rot |> rot
    |> t
  let p2 =
    (up (faceLine n |> mXZ |> fZ))
    |> fX |> mXZ |> rot |> rot
    |> t
  let x2 = 300.0 + 297.0 * cos (5.0 * pi / 6.0)
  let y2 = 300.0 - 297.0 * sin (5.0 * pi / 6.0)
  p1 @ List.tail p2
  |> toQs
  |> sprintf "%s\n       L 300 597"
  |> fun s -> sprintf "%s\n       A 297 297 0 0 1 %f %f" s x2 y2
  |> loop
  |> wrapPath style

let fixCorners a =
  let set i face path a =
    Array.set a i (faceColour face |> faceStyle |> path)
    a
  a
  |> set 3 North (nearCornerNorth 6)
  |> set 12 South (nearCornerSouth 6)
  |> set 21 East (nearCornerEast 6)

/// 3D manipulations
let mRotX v =
  baseM
    { x = 1.0; y = 0.0; z = 0.0 }
    { x = 0.0; y = cos v; z = -sin v }
    { x = 0.0; y = sin v; z = cos v }

let mRotY v =
  baseM
    { x = cos v; y = 0.0; z = sin v }
    { x = 0.0; y = 1.0; z = 0.0 }
    { x = -sin v; y = 0.0; z = cos v }

let mRotZ v =
  baseM
    { x = cos v; y = -sin v; z = 0.0 }
    { x = sin v; y = cos v; z = 0.0 }
    { x = 0.0; y = 0.0; z = 1.0 }

let turnM turn =
  match turn.face with
  | North -> mRotY
  | East -> mRotZ
  | South -> mRotX
  | West -> mRotY
  | Top -> mRotZ
  | Bottom -> mRotX

let render (model:Model) =
  match model.turning with
  | None ->
      (paths3D, model.colouring)
      ||> Array.zip
      |> Array.map (fun (path, face) -> colour (face |> faceColour) path)
      |> fixCorners
      |> Array.toList
  | Some ({turn = turn; started = _; progress = p}) ->
      let v = p * pi / 2.0
      (paths3D, model.colouring)
      ||> Array.zip
      |> Array.mapi (fun i (p,f) -> if indices.[turn.face] |> Array.contains i then (p |> (turnM turn v |> mul |> List.map), f) else (p,f))
      |> Array.map (fun (path, face) -> colour (face |> faceColour) path)
      |> fixCorners
      |> Array.toList

let view (model:Model) (dispatch: AppMsg -> unit) = 
    [ words 60 "Flat Rubik's cube"
      svg [ ClassName "faces"; Width (U2.Case1 600.0); (*Height (U2.Case1 600.0)*) unbox ("height", "600px") ] (render model)
    //   svg [ ClassName "faces" ] [
    //       path [ Fill "green"; D "M 10 10 L 30 30 L 10 30 Z" ] []
    //   ]
      buttonLink "" (fun _ -> dispatch (RubikMsg TurnNorthCW)) [ str "Turn north" ]
      buttonLink "" (fun _ -> dispatch (RubikMsg TurnSouthCW)) [ str "Turn south" ]
      buttonLink "" (fun _ -> dispatch (RubikMsg TurnEastCW)) [ str "Turn east" ]
      buttonLink "" (fun _ -> dispatch (RubikMsg TurnWestCW)) [ str "Turn west" ]
      buttonLink "" (fun _ -> dispatch (RubikMsg TurnTopCW)) [ str "Turn top" ]
      buttonLink "" (fun _ -> dispatch (RubikMsg TurnBottomCW)) [ str "Turn bottom" ]
    ]
