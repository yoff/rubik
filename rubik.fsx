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

let style = "\"stroke:black; stroke-width:3; fill:none\""

let wrapPath style =
  sprintf "  <path\n    style=%s\n    d=\"%s\"\n  />" style

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
    |> List.map (fun ((rx, ry), (cx, cy)) -> sprintf "<path\n  style=%s\n  d=\"M %f %f L %f %f\" />" style cx cy rx ry)
  [ yield sprintf "<circle cx=\"%f\" cy=\"%f\" r=\"%f\" style=%s />" x y radius style
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
    |> String.concat "\n    "
    |> sprintf "M %f %f\n    %s" x y

let toQPath style points =
  toQs points |> wrapPath style

let toQLoop style points =
  toQs points
  |> sprintf "%s\n   z"
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
  let dv = pi / 4.0 - cv
  let v i = cv + (float i)*dv/(float n)
  let l = dv / float n |> tanLen
  [0..n]
  |> List.map (fun i -> { x = -x; y = cos (v i); z = -sin (v i) })
  |> mapEveryOther false (fun v -> { x = v.x; y = l*v.y; z = l*v.z })

let centerLine n =
  let x = sin cv
  let dv = 2.0 * cv
  let rc = cos cv
  let v i = cv - (float i)*dv/(float n)
  let l = dv / float n |> tanLen
  [0..n]
  |> List.map (fun i -> { x = -x; y = cos (v i); z = rc * sin (v i) })
  |> mapEveryOther false (fun v -> { x = v.x; y = l*v.y; z = l*v.z })

let styles =
  [| "\"stroke:red; stroke-width:3; fill:none\""
     "\"stroke:blue; stroke-width:3; fill:none\"" 
     "\"stroke:green; stroke-width:3; fill:none\""
     "\"stroke:grey; stroke-width:3; fill:none\"" |]

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

let face style p1 p2 p3 p4 =
  p1 @ List.tail p2 @ List.tail p3 @ List.tail p4 |> toQLoop style

let faceStyle colour =
  sprintf "\"stroke:black; stroke-width:3; fill:%s\"" colour

[ cornerLine 4 |> List.map flipZ
  cornerLine 4 |> List.map rotate
  faceLine 4 |> List.map rotate
  faceLine 4 ]
|> List.mapi (fun i -> List.map (toView >> proj >> toScreen) >> toQPath styles.[i % 4])
|> String.concat "\n"
|> toSVG
|> saveSVG "facetest.svg"
  