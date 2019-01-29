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

let norm (v: V3) =
  v.x*v.x + v.y*v.y + v.z*v.z |> sqrt

let normalizeV v =
  let c = 1.0 / norm v
  scale c v

let dot v1 v2 =
  v1.x*v2.x + v1.y*v2.y + v1.z*v2.z

let vx =
  { x =  1.0
    y = -1.0 
    z =  0.0 }

let vy =
  { x = -0.5
    y = -0.5 
    z =  1.0 }

let vz =
  { x = 1.0
    y = 1.0 
    z = 1.0 }

dot vx vy
dot vy vz
dot vz vx

(*
 1 -.5 1 | 1 0 0
-1 -.5 1 | 0 1 0
 0 1   1 | 0 0 1
-----------------
 1 -.5 1 | 1 0 0
 0 -1  2 | 1 1 0
 0 1   1 | 0 0 1
-----------------
 1 -.5 1 | 1 0 0
 0 -1  2 | 1 1 0
 0 0   3 | 1 1 1
-----------------
 1  0 0 | .5 -.5 0
 0 -1 2 | 1   1 0
 0  0 3 | 1   1 1
-----------------
 1  0 0 | .5 -.5 0
 0 -1 2 | 1   1 0
 0  0 1 | 1/3 1/3 1/3
-----------------
 1  0 0 | .5 -.5 0
 0 -1 0 | 1/3 1/3 -2/3
 0  0 1 | 1/3 1/3 1/3
-----------------
 1 0 0 | .5 -.5 0
 0 1 0 | -1/3 -1/3 2/3
 0 0 1 | 1/3 1/3 1/3
*)
let bx =
  { x =  0.5
    y = -1.0/3.0 
    z =  1.0/3.0 }

let by =
  { x = -0.5
    y = -1.0/3.0 
    z =  1.0/3.0 }

let bz =
  { x = 0.0
    y = 2.0/3.0 
    z = 1.0/3.0 }

dot bx by
dot by bz
dot bz bx

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

let normalizeM m =
  { r1 = normalizeV m.r1
    r2 = normalizeV m.r2
    r3 = normalizeV m.r3 }

(*
let m = baseM vx vy vz |> normalizeM

mul m { x = 1.0; y = 0.0; z = 0.0 }
mul m { x = 0.0; y = 1.0; z = 0.0 }
mul m { x = 0.0; y = 0.0; z = 1.0 }
*)

let mr = baseM bx by bz
mul mr vx
mul mr vy
mul mr vz

// let m = baseM (normalizeV bx) (normalizeV by) (normalizeV bz)
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

let filter cutOff v =
  if v.z > cutOff then Some v else None

let cutOff = 0.06

let toView = add p >> mul m

let toPlane = toView >> filter cutOff >> Option.map proj

let toScreenCanvas xMin yMin xMax yMax targetWidth v =
  let width = xMax - xMin
  let height = yMax - yMin
  let scale = targetWidth / width
  (scale * (v.x-xMin), scale * (v.y-yMin))

let width = 600.0
let windowRadius = 2.5

let toScreen = toScreenCanvas -windowRadius -windowRadius windowRadius windowRadius width

let render = toPlane >> Option.map toScreen

let cosToSin c = 1.0 - c*c |> sqrt

let r cutOff =
  let c =
    { x = 0.0; y = 0.0; z = 0.0 }
    |> add p
    |> mul m

  let pcos = c.z - cutOff
  let psin = pcos |> cosToSin
  { x = psin; y = 0.0; z = cutOff }
  |> proj
  |> toScreen
  |> fst
  |> fun x -> x - 300.0

let style = "stroke:black; stroke-width:3; fill:none"

let wrapPath style =
  sprintf "  <path\n    style=\"%s\"\n    d=\"%s\"\n  />" style

let toPath =
  List.map (fun (x,y) -> sprintf "%f %f" x y)
  >> String.concat "\n       L "
  >> sprintf "M %s"
  >> wrapPath style

let toSVG =
  sprintf "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"600\" height=\"600\">\n%s\n</svg>"

let renderToPaths =
  List.map (List.choose render >> toPath)

let pathsToSVG : seq<string> -> string =
  String.concat "\n" >> toSVG

let renderToSVG =
  renderToPaths >> pathsToSVG

let paths =
  [
    [ { x = 1.0; y = 1.0; z = 1.0 }
      { x = -1.0; y = 1.0; z = 1.0 }
      { x = -1.0; y = -1.0; z = 1.0 }
    ]
    [ { x = 1.0; y = 1.0; z = 1.0 }
      { x = 1.0; y = -1.0; z = 1.0 }
      { x = 1.0; y = -1.0; z = -1.0 }
    ]
    [ { x = 1.0; y = 1.0; z = 1.0 }
      { x = 1.0; y = 1.0; z = -1.0 }
      { x = -1.0; y = 1.0; z = -1.0 }
    ]
  ]

let svg = renderToSVG paths

let localPath = __SOURCE_DIRECTORY__
let svgPath fileName = System.IO.Path.Combine(localPath,fileName)
let saveSVG fileName svg = System.IO.File.WriteAllText (svgPath fileName, svg)

saveSVG "rubikTest.svg" svg

let pi = System.Math.PI

let rotate v =
  { x = v.z; y = v.x; z = v.y }
let rotations vs =
  [ vs 
    vs |> List.map rotate
    vs |> List.map (rotate >> rotate)
  ]

let circles d =
  let circle n d =
    let v i =
      2.0*pi*(float i)/(float n)
    let r = d |> cosToSin
    [0..n]
    |> List.map (fun i -> { x = r*cos (v i); y = r*sin (v i); z = d })
  [-d; d]
  |> List.collect (circle 60 >> rotations)

let svgCircles = 0.15 |> circles |> renderToSVG
saveSVG "rubikTestCircles.svg" svgCircles

let flipX v =
  { x = -v.x; y = v.y; z = v.z }
let flipY v =
  { x = v.x; y = -v.y; z = v.z }
let flipZ v =
  { x = v.x; y = v.y; z = -v.z }

let lines =
  let line =
    [-1.0..0.1..1.0]
    |> List.map (fun i -> { x = i; y = 1.0; z = 1.0})
    |> List.map normalizeV
  [ line
    line |> List.map flipY
    line |> List.map flipZ
    line |> List.map (flipY >> flipZ)
  ] |> List.collect rotations

let cd = 0.15
let rubik =
  (cd |> circles) @ lines

let rim =
  let x, y = toScreen { x = 0.0; y = 0.0; z = 0.0 }
  let radius = width / 2.0 - 3.0
  
  let r = cd |> cosToSin
  let s = sqrt 0.5

  let cp = { x = r*s; y = r*s; z = cd }
  let clines =
    [ cp
      cp |> rotate
      cp |> rotate |> rotate ]
    |> List.map (toView >> proj >> toScreen)
    |> List.zip ([ 0.5*pi; -pi/6.0; -5.0*pi/6.0 ] |> List.map (fun v -> (x + radius*cos v, y + radius*sin v)))
    |> List.map (fun ((rx, ry), (cx, cy)) -> sprintf "<path\n  style=\"%s\"\n  d=\"M %f %f L %f %f\" />" style cx cy rx ry)
  [ yield sprintf "<circle cx=\"%f\" cy=\"%f\" r=\"%f\" style=\"%s\" />" x y radius style
    yield! clines
  ]

let svgRubik =
  [ yield! rubik |> renderToPaths
    yield! rim
  ] |> pathsToSVG

saveSVG "rubikTestFull.svg" svgRubik

let rec mapEveryOther first f l =
  let mapFirst x =
    if first then f x else x
  match l with
  | [] -> []
  | x::xs -> mapFirst x :: mapEveryOther (not first) f xs

let tanLen v =
   (tan v)*(tan v) + 1.0 |> sqrt

let arcTest n r =
  let v = pi / (2.0 * float n)
  let s = (tan v)*(tan v) + 1.0 |> sqrt
  [0..n]
  |> List.map (fun i -> r * cos (float i*v), r * sin (float i*v))
  |> mapEveryOther false (fun (x,y) -> (s*x, s*y))

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

let toQPath style points =
  toQs points |> wrapPath style

let toQLoop style points =
  toQs points
  |> sprintf "%s\n       z"
  |> wrapPath style

arcTest 4 100.0
|> toQPath style
|> sprintf "  <circle cx=\"0\" cy=\"0\" r=\"100\" style=\"stroke:white; stroke-width:5; fill:none\" />\n%s"
|> sprintf "  <circle cx=\"0\" cy=\"0\" r=\"100\" style=\"stroke:red; stroke-width:7; fill:none\" />\n%s"
|> toSVG
|> saveSVG "arcTest.svg"

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

let styles =
  [| "stroke:red; stroke-width:3; fill:none"
     "stroke:blue; stroke-width:3; fill:none" 
     "stroke:green; stroke-width:3; fill:none"
     "stroke:grey; stroke-width:3; fill:none" |]

let qtest =
  [ cornerLine 4
    edgeLine 4
    faceLine 4
    centerLine 4
  ]
  |> List.collect rotations
  |> List.mapi (fun i -> List.map (toView >> proj >> toScreen) >> toQPath styles.[i % 3])

[ yield! rubik |> renderToPaths
  yield! rim
  yield! qtest
]
|> String.concat "\n"
|> toSVG
|> saveSVG "qtest.svg"

let toFace p1 p2 p3 p4 =
  p1 @ List.tail p2 @ List.tail p3 @ List.tail p4

let face style p1 p2 p3 p4 =
  toFace p1 p2 p3 p4 |> toQLoop style

let mirrorXY v =
  { x = v.y; y = v.x; z = v.z }

let mirrorXZ v =
  { x = v.z; y = v.y; z = v.x }

let mirrorYZ v =
  { x = v.x; y = v.z; z = v.y }

let faceStyle colour =
  sprintf "stroke:black; stroke-width:3; fill:%s" colour

[ cornerLine 4 |> List.map (flipY >> flipX)
  cornerLine 4 |> List.map rotate
  faceLine 4 |> List.map (mirrorXY >> rotate)
  faceLine 4 ]
|> List.mapi (fun i -> List.map (toView >> proj >> toScreen) >> toQPath styles.[i % 4])
|> String.concat "\n"
|> toSVG
|> saveSVG "facetest.svg"

let t = toView >> proj >> toScreen |> List.map
face (faceStyle "green")
  (cornerLine 4 |> List.map (flipY >> flipX) |> t)
  (cornerLine 4 |> List.map rotate |> t |> List.rev)
  (faceLine 4 |> t |> List.rev)
  (faceLine 4 |> List.map (mirrorXY >> rotate) |> t)
|> toSVG
|> saveSVG "facetest1.svg"

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

//let nearCorner n =

let choosei p =
  List.mapi (fun i e -> i, e)
  >> List.choose (fun (i,e) -> if p i then Some e else None)

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

let colour c = t >> toQLoop (faceStyle c)
let colourAll c = colour c |> List.map

[ yield! corners (corner 6) |> colourAll "green"
  yield! corners (corner 6) |> List.map rot |> colourAll "orange"
  yield! corners (corner 6) |> List.map (rot >> rot) |> colourAll "purple"
  yield! corners (corner 6 |> fY) |> colourAll "red"
  yield! corners (corner 6 |> fY) |> List.map rot |> colourAll "blue"
  yield! corners (corner 6 |> fY) |> List.map (rot >> rot) |> colourAll "yellow"
  yield! [center 6] |> colourAll "green"
  yield! [center 6] |> List.map rot |> colourAll "orange"
  yield! [center 6] |> List.map (rot >> rot) |> colourAll "purple"
  yield! [center 6] |> List.map fY |> colourAll "red"
  yield! [center 6] |> List.map (fY >> rot) |> colourAll "blue"
  yield! [center 6] |> List.map (fY >> rot >> rot) |> colourAll "yellow"
  yield! edges (edge 6) |> colourAll "green"
  yield! edges (edge 6) |> List.map rot |> colourAll "orange"
  yield! edges (edge 6) |> List.map (rot >> rot) |> colourAll "purple"
  yield! edges (edge 6 |> fY) |> colourAll "red"
  yield! edges (edge 6 |> fY) |> List.map rot |> colourAll "blue"
  yield! edges (edge 6 |> fY) |> List.map (rot >> rot) |> colourAll "yellow"
  yield! rubik |> renderToPaths
  yield! rim
]
|> choosei (fun i -> i <> 3 && i <> 7 && i <> 11)
|> String.concat "\n"
|> toSVG
|> saveSVG "facetest2.svg"

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

let rotPaths i =
  List.map (2.0 * pi * (float i) / 36.0 |> mRotX |> mul)
  >> t
  >> toQPath style

[1..36]
|> List.collect (fun i ->
               [ faceLine 6 |> mXY |> rot
                 cornerLine 6 |> rot
                 centerLine 6 |> mXZ
                 edgeLine 6 |> mXZ
               ]
               |> List.map (rotPaths i)
            )
|> String.concat "\n"
|> toSVG
|> saveSVG "rottest.svg"

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

type Model =
  { couloring: Face[]
    turn: (Turn * float) option }

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

let initialColouring =
  [| North
     East
     South
     West
     Top
     Bottom
  |]
  |> Array.collect (Array.create 9)

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

// let mapFace face f paths =
//   match face with
//   | North -> [|]

let centroid (points: (float * float) list) =
  let n = points.Length |> float
  let cx, cy =
    points
    |> List.fold (fun (cx, cy) (x, y) -> (cx+x, cy+y)) (0.0, 0.0)
  (cx/n, cy/n)

let faceColour = function
  | North -> "green"
  | East -> "orange"
  | South -> "purple"
  | West -> "red"
  | Top -> "blue"
  | Bottom -> "yellow"

let colourNumber n (path, face) =
  let contour = t path
  let style = face |> faceColour |> faceStyle
  let filledFace =
    if n = 3 || n = 12 || n = 21 then "" else
    contour |> toQLoop style
  let number =
    contour
    |> centroid
    |> fun (x,y) -> sprintf "<text x=\"%f\" y=\"%f\" text-anchor=\"middle\" style=\"%s\">%d</text>" x y style n
  sprintf "%s\n%s" filledFace number

(paths3D, initialColouring)
||> Array.zip
|> Array.mapi colourNumber
|> String.concat "\n"
|> toSVG
|> saveSVG "numbering.svg"

let testFace face fileName =
  (paths3D, initialColouring)
  ||> Array.zip
  |> Array.map (fun (path, face) -> colour (face |> faceColour |> faceStyle) path)
  |> Array.mapi (fun i s -> if indices.[face] |> Array.contains i then s else "")
  |> String.concat "\n"
  |> toSVG
  |> saveSVG fileName

testFace North "testNorth.svg"
testFace South "testSouth.svg"
testFace East "testEast.svg"
testFace West "testWest.svg"
testFace Top "testTop.svg"
testFace Bottom "testBottom.svg"

let turnM turn =
  match turn.face with
  | North -> mRotY
  | East -> mRotZ
  | South -> mRotX
  | West -> mRotY
  | Top -> mRotZ
  | Bottom -> mRotX

let testFaceTurn turn v fileName =
  (paths3D, initialColouring)
  ||> Array.zip
  |> Array.mapi (fun i (p,f) -> if indices.[turn.face] |> Array.contains i then (p |> (turnM turn v |> mul |> List.map), f) else (p,f))
  |> Array.map (fun (path, face) -> colour (face |> faceColour |> faceStyle) path)
  |> String.concat "\n"
  |> toSVG
  |> saveSVG fileName

// testFaceTurn { face = North; direction = Clockwise } (pi/4.0) "testTurnNorth.svg"
// testFaceTurn { face = East; direction = Clockwise } (pi/4.0) "testTurnEast.svg"
// testFaceTurn { face = South; direction = Clockwise } (pi/4.0) "testTurnSouth.svg"
testFaceTurn { face = West; direction = Clockwise } (pi/4.0) "testTurnWest.svg"
testFaceTurn { face = Top; direction = Clockwise } (pi/4.0) "testTurnTop.svg"
testFaceTurn { face = Bottom; direction = Clockwise } (pi/4.0) "testTurnBottom.svg"