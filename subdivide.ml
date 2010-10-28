let icosahedron_tris =
  let phi = (1. +. (sqrt 5.)) /. 2. in
  let a, b = 1.0, phi in
  let r = (sqrt (1 +. (phi *. phi))) /. 2. in
  let a, b = a /. r, b /. r in

  let points = [|
     0., -.a, -.b;  0.,   a, -.b;  0.,   a,   b;   0., -.a,   b;
    -.b,  0.,   a;   b,  0.,   a;   b,  0., -.a;  -.b,  0., -.a;
    -.a, -.b,  0.;   a, -.b,  0.;   a,   b,  0.;  -.a,   b,  0.;
  |] in

  let tris = [
    1, 2, 10;  1, 11, 2;   0, 9, 3;   3, 8, 0;
    5, 11, 8;  11, 4, 8;  9, 10, 6;  9, 7, 10;
     5, 6, 2;   5, 3, 6;   4, 1, 7;   7, 0, 4;
    5, 2, 11;  6, 10, 2;  10, 7, 1;  1, 4, 11;
     5, 8, 3;   6, 3, 9;   0, 7, 9;   8, 4, 0;
  ] in

  List.map (fun (a,b,c) ->
    ( Array.get points a,
      Array.get points b,
      Array.get points c )
  ) tris

let sphere_uv p =
  let x,y,z = dir p in
  let phi = acos y in
  let theta = acos (z /. (sin phi)) in
  let u = phi in
  let v = if x > 0. then theta else 1. -. theta in
  (u, v)

let rec subdivide_sphere (o,r) (a,b,c) levels =
  if levels == 0
    then ((a,b,c), []) else
  let edge_point a b =
    dir ((a +^ b) /^ 2.) in
  let levels = levels - 1 in
  let u = edge_point a b in
  let v = edge_point b c in
  let w = edge_point c a in
  (vmap (fun x -> x *^ r +^ o) (a,b,c)),
  ( sphere_uv a,
    sphere_uv b,
    sphere_uv c ),
  [ subdivide_sphere (u, v, w) levels;
    subdivide_sphere (a, u, w) levels;
    subdivide_sphere (b, v, u) levels;
    subdivide_sphere (c, w, v) levels;
  ]

let make_sphere_tree (c,r) n = List.map (fun t -> subdivide_sphere (c,r) t n) icosahedron_tris