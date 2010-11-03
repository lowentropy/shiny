open Graphics
open Types
open Consts
open Math
open Draw
open Int
open Shaders
open Trace
open Materials
open Builders
open Subdivide

let make_plane plane = (plane_vol plane, int_plane plane)
let make_sphere sphere = (sphere_vol sphere, int_sphere sphere)
let make_cyl cyl capped = (cyl_vol cyl, int_cyl cyl capped)
let make_tri t = (tri_vol t, int_tri t)
let make_obj shape surface substance = Object (shape, surface, substance)
let r () = Random.float 1.0

(* shapes *)
let ground  = (0.0,  1.0,  0.0),  1.0

(* lights *)
let lights = make_list 10 (fun i ->
  point_light (r()*.10.-.5.,12.,r()*.10.-.5.) (white -^ (r(),r(),r()) *^ 0.2) 40.0
)

(* objects *)
let ground_obj = make_obj (make_plane ground) dull_white None
let tri_objs = List.map (fun t -> make_obj (make_tri t) mirror None) (subdivided_sphere_tris ((0.,0.,0.),1.) 0)

(* render params *)
let complexity_view = false
let width = 400 (* 1680 *)
let height = 400 (* 1050 *)
let jitter = 0.2 (* 0.2 *)
let numrays = 1
let aspect = (float width) /. (float height)
let fovy = 2. *. (atan 1.)
let fovx = 2. *. (atan aspect)

let steps = 50

let updater angle =
  let angle = angle +. (2. *. pi) /. (float steps) in
  if angle >= (2. *. pi) then None else Some angle

let drawer angle =
  let camloc = (3. *. (cos angle), 3., 3. *. (sin angle)) in
  let camdir  = zv -^ dir camloc in
  let cam = (camloc, camdir, (0.,1.,0.), fovx, fovy) in
  let scene = cam, [ground_obj] @ tri_objs @ lights in
  draw_scene false jitter numrays scene
  
;;

seed();
draw_movie width height drawer updater 0.
