module App

(**
 The famous Increment/Decrement ported from Elm.
 You can find more info about Emish architecture and samples at https://elmish.github.io/
*)
open System

open Elmish
open Elmish.React
open Fable.Helpers.React
open Fable.Helpers.React.Props

open Fable.Import.Browser
// open Fable.Core.JsInterop

/// Model

type Face =
  | NearY
  | NearZ
  | NearX
  | FarY
  | FarZ
  | FarX

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
  [| NearY
     NearZ
     NearX
     FarY
     FarZ
     FarX
  |]
  |> Array.collect (Array.create 9)

let init () =
  { colouring = initialColouring
    turning = None }

/// Messages - not used yet

type Message =
  | Turn of Turn
  | Tick of DateTime

/// Animation
let timerTick dispatch =
    window.setInterval(fun _ -> 
        dispatch (Tick DateTime.Now)
    , 100) |> ignore

let timerSub _ = Cmd.ofSub timerTick

let turnNearYCCW = Turn { face = NearY; direction = CounterClockwise}
let turnNearYCW = Turn { face = NearY; direction = Clockwise}
let turnFarYCCW = Turn { face = FarY; direction = CounterClockwise}
let turnFarYCW = Turn { face = FarY; direction = Clockwise}
let turnNearXCCW = Turn { face = NearX; direction = CounterClockwise}
let turnNearXCW = Turn { face = NearX; direction = Clockwise}
let turnFarXCCW = Turn { face = FarX; direction = CounterClockwise}
let turnFarXCW = Turn { face = FarX; direction = Clockwise}
let turnNearZCCW = Turn { face = NearZ; direction = CounterClockwise}
let turnNearZCW = Turn { face = NearZ; direction = Clockwise}
let turnFarZCCW = Turn { face = FarZ; direction = CounterClockwise}
let turnFarZCW = Turn { face = FarZ; direction = Clockwise}

let mapKey keyCode =
  match keyCode with
  | 81.0 -> Some turnNearYCCW // Q
  | 65.0 -> Some turnNearYCW // A
  | 87.0 -> Some turnFarYCCW // W
  | 83.0 -> Some turnFarYCW // S
  | 89.0 -> Some turnNearXCW // Y
  | 71.0 -> Some turnNearXCCW // G
  | 84.0 -> Some turnFarXCW // T
  | 70.0 -> Some turnFarXCCW // F
  | 90.0 -> Some turnNearZCCW // Z
  | 86.0 -> Some turnNearZCW // v
  | 88.0 -> Some turnFarZCCW // X
  | 67.0 -> Some turnFarZCW // C
  | _ -> None

let keyEvent dispatch =
  window.addEventListener_keydown(fun e ->
    match mapKey e.keyCode with
    | Some msg -> dispatch msg
    | None -> ()
    :> obj)

let keySub _ = Cmd.ofSub keyEvent

/// Scramble
let scramble dispatch =
  let n = 20 // this many random moves
  let r = System.Random ()
  let genMove _ =
    let face = 
      match r.Next (6) with
      | 0 -> NearY
      | 1 -> FarY
      | 2 -> NearX
      | 3 -> FarX
      | 4 -> NearZ
      | 5 -> FarZ
      | _ -> NearY // Should not happen
    let direction =
      match r.Next (2) with
      | 0 -> Clockwise
      | 1 -> CounterClockwise
      | _ -> Clockwise // Should not happen
    Turn { face = face; direction = direction }
  [1..n]
  |> List.map genMove
  |> List.iter dispatch

/// Update
let applyPermutation p (colouring: 'a[]) =
  p |> Array.map (fun i -> colouring.[i])

let flipPerm p =
  p
  |> applyPermutation p
  |> applyPermutation p

let permNearYCW =
  [|
      yield! [| 0..8 |]
      yield! [| 20; 18; 11; 12; 24; 14; 23; 22; 26 |]
      yield! [| 36; 19; 37; 21; 42; 43; 40; 25; 44 |]
      yield! [| 28; 30; 27; 29; 34; 33; 31; 32; 35 |]
      yield! [| 47; 45; 38; 39; 51; 41; 50; 49; 53 |]
      yield! [|  9; 46; 10; 48; 15; 16; 13; 52; 17 |]
  |]

let permNearYCCW =
  flipPerm permNearYCW

let permFarYCW =
  [|
      yield! [| 0..8 |]
      yield! [| 20; 18; 11; 12; 24; 14; 15; 16; 17 |]
      yield! [| 36; 19; 37; 21; 22; 23; 40; 25; 26 |]
      yield! [| 28; 30; 27; 29; 34; 33; 31; 32; 35 |]
      yield! [| 47; 45; 38; 39; 51; 41; 42; 43; 44 |]
      yield! [|  9; 46; 10; 48; 49; 50; 13; 52; 53 |]
  |]

let permFarYCCW =
  flipPerm permFarYCW

let permNearXCW =
  [|
      yield! [| 11;  9;  2;  3; 15;  5; 14; 13; 17 |]
      yield! [| 27; 10; 28; 12; 33; 34; 31; 16; 35 |]
      yield! [| 18..26 |]
      yield! [| 38; 36; 29; 30; 42; 32; 41; 40; 44 |]
      yield! [|  0; 37;  1; 39;  6;  7;  4; 43;  8 |]
      yield! [| 46; 48; 45; 47; 52; 51; 49; 50; 53 |]
  |]

let permNearXCCW =
  flipPerm permNearXCW

let permFarXCW =
  [|
      yield! [| 11;  9;  2;  3; 15;  5;  6;  7;  8 |]
      yield! [| 27; 10; 28; 12; 13; 14; 31; 16; 17 |]
      yield! [| 18..26 |]
      yield! [| 38; 36; 29; 30; 42; 32; 33; 34; 35 |]
      yield! [|  0; 37;  1; 39; 40; 41;  4; 43; 44 |]
      yield! [| 46; 48; 45; 47; 52; 51; 49; 50; 53 |]
  |]

let permFarXCCW =
  flipPerm permFarXCW

let permNearZCW =
  [|
      yield! [| 45;  1; 46;  3; 51; 52; 49;  7; 53 |]
      yield! [| 9..17 |]
      yield! [|  2;  0; 20; 21;  6; 23;  5;  4;  8 |]
      yield! [| 18; 28; 19; 30; 24; 25; 22; 34; 26 |]
      yield! [| 37; 39; 36; 38; 43; 42; 40; 41; 44 |]
      yield! [| 29; 27; 47; 48; 33; 50; 32; 31; 35 |]
  |]

let permNearZCCW =
  flipPerm permNearZCW

let permFarZCW =
  [|
      yield! [| 45;  1; 46;  3;  4;  5; 49;  7;  8 |]
      yield! [| 9..17 |]
      yield! [|  2;  0; 20; 21;  6; 23; 24; 25; 26 |]
      yield! [| 18; 28; 19; 30; 31; 32; 22; 34; 35 |]
      yield! [| 37; 39; 36; 38; 43; 42; 40; 41; 44 |]
      yield! [| 29; 27; 47; 48; 33; 50; 51; 52; 53 |]
  |]

let permFarZCCW =
  flipPerm permFarZCW

let apply turn (colouring: Face[]) =
  let perm =
    match turn.face, turn.direction with
    | NearY, Clockwise -> permNearYCW
    | NearY, CounterClockwise -> permNearYCCW
    | FarY, Clockwise -> permFarYCW
    | FarY, CounterClockwise -> permFarYCCW
    | NearX, Clockwise -> permNearXCW
    | NearX, CounterClockwise -> permNearXCCW
    | FarX, Clockwise -> permFarXCW
    | FarX, CounterClockwise -> permFarXCCW
    | NearZ, Clockwise -> permNearZCW
    | NearZ, CounterClockwise -> permNearZCCW
    | FarZ, Clockwise -> permFarZCW
    | FarZ, CounterClockwise -> permFarZCCW
  applyPermutation perm colouring

// milliseconds it takes to complete a turn
let turnTime = 1000.0 // 1 second

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

let update (msg:Message) model : Model =
  let setTurning turning =
    match model.turning with
    | None -> 
      { model with turning = Some turning }
    | Some currentlyTurning ->
      { colouring = apply currentlyTurning.turn model.colouring
        turning = Some turning }
  match msg with
  | Tick t ->
    updateTick t model
  | Turn turn ->
    let turning = { turn = turn; started = System.DateTime.Now; progress = 0.0 }
    setTurning turning

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
  [ NearY,
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
  ; NearZ,
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
  ; NearX,
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
  ; FarY,
    [| yield! moveFace 1 edgeLeft
       yield! moveFace 2 edgeRight
       yield! moveFace 3 [|0..8|]
       yield! moveFace 4 edgeLeft
       yield! moveFace 5 edgeRight
    |]
    // |> Array.sort
  ; FarZ,
    [| yield! edgeRight
       yield! moveFace 2 edgeLeft
       yield! moveFace 3 edgeRight
       yield! moveFace 4 [|0..8|]
       yield! moveFace 5 edgeLeft
    |]
    // |> Array.sort
  ; FarX,
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
  | [] | [_] | [_;_] -> failwith "Too few points (need at lNearZ three)"
  | (x,y)::ps ->
    ps
    |> pairs
    |> List.map (fun ((cx, cy), (px,py)) -> sprintf "Q %f %f %f %f" cx cy px py)
    |> String.concat "\n       "
    |> sprintf "M %f %f\n       %s" x y

let wrapPath style p =
  path ((ClassName style :> IProp) :: [D p]) [] 

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

let FarZlane = toView >> proj

let toScreenCanvas xMin yMin xMax yMax targetWidth v =
  let width = xMax - xMin
  let height = yMax - yMin
  let scale = targetWidth / width
  (scale * (v.x-xMin), scale * (v.y-yMin))

let width = 600.0
let windowRadius = 2.5

let toScreen = toScreenCanvas -windowRadius -windowRadius windowRadius windowRadius width
let t = FarZlane >> toScreen |> List.map

let faceColour = function
  | NearY -> "NearY"
  | NearZ -> "NearZ"
  | NearX -> "NearX"
  | FarY -> "FarY"
  | FarZ -> "FarZ"
  | FarX -> "FarX"
  
let colour c = t >> toQLoop c

let halfWidth = width / 2.0
let inset = 3.0
let insetHalfWidth = halfWidth - inset
let nearCorner v1 v2 m n style =
  let p1 =
    (down (faceLine n |> mXZ |> fZ))
    |> fX |> m
    |> t
  let p2 =
    (up (faceLine n |> mXZ |> fZ))
    |> fX |> mXZ |> m
    |> t
  let x1 = halfWidth + insetHalfWidth * cos (v1 * pi)
  let y1 = halfWidth - insetHalfWidth * sin (v1 * pi)
  let x2 = halfWidth + insetHalfWidth * cos (v2 * pi)
  let y2 = halfWidth - insetHalfWidth * sin (v2 * pi)
  p1 @ List.tail p2
  |> toQs
  |> fun s -> sprintf "%s\n       L %f %f" s x1 y1
  |> fun s -> sprintf "%s\n       A %f %f 0 0 1 %f %f" s insetHalfWidth insetHalfWidth x2 y2
  |> loop
  |> wrapPath style

let nearCornerY = nearCorner (1.0 / 6.0) -0.5 id
let nearCornerZ = nearCorner (5.0 / 6.0) (1.0 / 6.0) rot
let nearCornerX = nearCorner -0.5 (5.0 / 6.0) (rot >> rot)

let fixCorners a =
  let set i face path a =
    Array.set a i (faceColour face |> path)
    a
  a
  |> set 3 NearY (nearCornerY 6)
  |> set 12 NearX (nearCornerX 6)
  |> set 21 NearZ (nearCornerZ 6)

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
  | NearY -> mRotY
  | NearZ -> mRotZ
  | NearX -> mRotX
  | FarY -> mRotY
  | FarZ -> mRotZ
  | FarX -> mRotX

let render (model:Model) =
  match model.turning with
  | None ->
      (paths3D, model.colouring)
      ||> Array.zip
      |> Array.map (fun (path, face) -> colour (face |> faceColour) path)
      |> fixCorners
      |> Array.toList
  | Some ({turn = turn; started = _; progress = p}) ->
      let sign = match turn.direction with Clockwise -> 1.0 | CounterClockwise -> -1.0
      let v = sign * p * pi / 2.0
      (paths3D, model.colouring)
      ||> Array.zip
      |> Array.mapi (fun i (p,f) -> if indices.[turn.face] |> Array.contains i then (p |> (turnM turn v |> mul |> List.map), f) else (p,f))
      |> Array.map (fun (path, face) -> colour (face |> faceColour) path)
      |> fixCorners
      |> Array.toList

/// Arrow buttons
let largeArrow =
  "M 0 0
   A 40 40 0 0 0 20 -20
   A 30 30 0 0 0 12 -23
   A 50 50 0 0 0 -65 -63
   A 40 40 0 0 1 -12 -23
   A 30 30 0 0 0 -20 -20
   A 40 40 0 0 0 0 0
   Z"

let smallArrow =
  "M 0 0
   A 32 32 0 0 0 16 -16
   A 24 24 0 0 0 10 -18
   A 40 40 0 0 0 -52 -50
   A 32 32 0 0 1 -10 -18
   A 24 24 0 0 0 -16 -16
   A 32 32 0 0 0 0 0
   Z"

let arrowsFarX dispatch turnLarge turnSmall =
  [ g
      [ SVGAttr.Transform "translate(45,95),rotate(40,0,0)" //:> IProp
        OnClick (fun _ -> dispatch turnLarge) //:> IProp
      ]
      [ path [ D largeArrow ] [] ]
    g
      [ SVGAttr.Transform "translate(7,77),rotate(55,0,0)" :> IProp
        OnClick (fun _ -> dispatch turnSmall) :> IProp ]
      [ path [ D smallArrow ] [] ]
  ]

let arrows dispatch =
  [ g [ SVGAttr.Transform "translate(300,600)" ] (arrowsFarX dispatch turnNearZCW turnFarZCW)
    g [ SVGAttr.Transform "translate(300,600),scale(-1,1)" ] (arrowsFarX dispatch turnNearZCCW turnFarZCCW)
    g [ SVGAttr.Transform "translate(300,600),rotate(120,0,-300)" ] (arrowsFarX dispatch turnNearYCW turnFarYCW)
    g [ SVGAttr.Transform "translate(300,600),rotate(120,0,-300),scale(-1,1)" ] (arrowsFarX dispatch turnNearYCCW turnFarYCCW)
    g [ SVGAttr.Transform "translate(300,600),rotate(240,0,-300)" ] (arrowsFarX dispatch turnNearXCW turnFarXCW)
    g [ SVGAttr.Transform "translate(300,600),rotate(240,0,-300),scale(-1,1)" ] (arrowsFarX dispatch turnNearXCCW turnFarXCCW)
  ]

let shortcutsFarX tInv largeCW largeCCW smallCW smallCCW =
  [ g
      [ SVGAttr.Transform ((15, 42, tInv) |||> sprintf "translate(%d,%d),%s") ]
      [ text [ SVGAttr.TextAnchor "middle"; SVGAttr.Custom ("alignment-baseline", "middle"); Y 5.0 ] [ str smallCCW ] ]
    g
      [ SVGAttr.Transform ((-15, 42, tInv) |||> sprintf "translate(%d,%d),%s") ]
      [ text [ SVGAttr.TextAnchor "middle"; SVGAttr.Custom ("alignment-baseline", "middle"); Y 5.0 ] [ str smallCW ] ]
    g
      [ SVGAttr.Transform ((83, 86, tInv) |||> sprintf "translate(%d,%d),%s") ]
      [ text [ SVGAttr.TextAnchor "middle"; SVGAttr.Custom ("alignment-baseline", "middle"); Y 5.0 ] [ str largeCCW ] ]
    g
      [ SVGAttr.Transform ((-83, 86, tInv) |||> sprintf "translate(%d,%d),%s") ]
      [ text [ SVGAttr.TextAnchor "middle"; SVGAttr.Custom ("alignment-baseline", "middle"); Y 5.0 ] [ str largeCW ] ]
  ]

let shortcuts =
  [ g [ SVGAttr.Transform "translate(300,600)" ] (shortcutsFarX "rotate(0,0,0)" "Z" "V" "X" "C")
    g [ SVGAttr.Transform "translate(300,600),rotate(120,0,-300)" ] (shortcutsFarX "rotate(-120,0,0)" "Q" "A" "W" "S")
    g [ SVGAttr.Transform "translate(300,600),rotate(240,0,-300)" ] (shortcutsFarX "rotate(-240,0,0)" "G" "Y" "F" "T")
  ]

let composition model (dispatch: Message -> unit) =
  [ g [ SVGAttr.Transform "translate(100,0)" ]
      [ g [ ClassName "faces" ] (render model)
        g [ ClassName "arrows" ] (arrows dispatch)
        g [ ClassName "shortcuts" ] shortcuts ]
  ]

let view (model:Model) (dispatch: Dispatch<Message>) = 
  div [ Class "main" ]
    [ h1 [ Class "title"] [str "Flat Rubik's cube"]
      svg [Class "centered"; SVGAttr.Width 800.0; SVGAttr.Height 700.0 ] (composition model dispatch)
      div [ Class "spacer" ] []
      button [ Class "centered"; OnClick (fun _ -> scramble dispatch) ] [ str "Scramble!" ]
    ]

// App
Program.mkSimple init update view
|> Program.withSubscription timerSub
|> Program.withSubscription keySub
|> Program.withReact "elmish-app"
|> Program.withConsoleTrace
|> Program.run