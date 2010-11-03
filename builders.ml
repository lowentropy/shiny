open Types
open Int

let make_plane plane = (plane_vol plane, int_plane plane)
let make_sphere sphere = (sphere_vol sphere, int_sphere sphere)
let make_cyl cyl capped = (cyl_vol cyl, int_cyl cyl capped)
let make_obj shape surface substance = Object (shape, surface, substance)

let icosahedron_tris =
  let phi = (1. +. (sqrt 5.)) /. 2. in
  let a, b = 1.0, phi in
  let r = (sqrt (1. +. (phi *. phi))) /. 2. in
  let a, b = a /. r, b /. r in

  let points = [|
    -.b,   0., a ; b,   0., a ; b,  0., -.a ; -.b,    0., -.a ;
    -.a,  -.b, 0.; a, -.b,  0.; a,  b,    0.; -.a,    b,    0.;
      0., -.a, b ; 0.,  a,  b ; 0., a,  -.b ;   0., -.a,  -.b ;
  |] in

  let tris = [
     0, 8, 9;  9, 8, 1; 11,10, 2; 3,10,11;
     1, 2, 6;  5, 2, 1;  3, 4, 0; 0, 7, 3;
     7, 6,10;  9, 6, 7;  4, 5, 8; 4,11, 5;
     9, 1, 6;  6, 2,10; 10, 3, 7; 7, 0, 9;
     5, 1, 8; 11, 2, 5;  3,11, 4; 8, 0, 4;
  ] in

  List.map (fun (a,b,c) ->
    ( Array.get points a,
      Array.get points b,
      Array.get points c )
  ) tris
