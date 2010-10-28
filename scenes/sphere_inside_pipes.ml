open Graphics
open Types
open Consts
open Math
open Draw
open Int
open Shaders
open Trace
open Materials

let complexity_view = true

let make_plane plane = (plane_vol plane, int_plane plane)
let make_sphere sphere = (sphere_vol sphere, int_sphere sphere)
let make_cyl cyl capped = (cyl_vol cyl, int_cyl cyl capped)
let make_obj shape surface substance = Object (shape, surface, substance)

(* shapes *)
let ground  = (0.0,  1.0,  0.0),  5.0
let wall1 = dir (1.0, 0.0, 20.0), 4.5
let wall2 = (0.0, 0.0, -1.0), 4.5

let r = 1.
let l = r *. 2.
let h = sqrt 3.
let a = 1. /. h
let b = h -. a
let tetrahedron = [
  (0.,  1., -.b);
  (1.,  1., a);
  (-1., 1., a);
  (0., 1. +. (sqrt (l *. l -. b *. b)), 0.)
]
let spheres = List.map (fun p -> (p *^ r, r)) tetrahedron

let cube = [
  ( 1., 1., 1.);
  ( 1., 1.,-1.);
  (-1., 1.,-1.);
  (-1., 1., 1.);
  ( 1.,-1., 1.);
  ( 1.,-1.,-1.);
  (-1.,-1.,-1.);
  (-1.,-1., 1.);
]

let cube_spheres = List.map (fun p -> (p *^ (r *. 4.), r /. 2.)) cube
let edges = [
  (( 1., 1., 1.), ( 1., 1.,-1.));
  (( 1., 1.,-1.), (-1., 1.,-1.));
  ((-1., 1.,-1.), (-1., 1., 1.));
  ((-1., 1., 1.), ( 1., 1., 1.));
  (( 1.,-1., 1.), ( 1.,-1.,-1.));
  (( 1.,-1.,-1.), (-1.,-1.,-1.));
  ((-1.,-1.,-1.), (-1.,-1., 1.));
  ((-1.,-1., 1.), ( 1.,-1., 1.));
  (( 1., 1., 1.), ( 1.,-1., 1.));
  (( 1., 1.,-1.), ( 1.,-1.,-1.));
  ((-1., 1.,-1.), (-1.,-1.,-1.));
  ((-1., 1., 1.), (-1.,-1., 1.));
]
let cyls = List.map (fun (a,b) -> (a *^ (r *. 4.), b *^ (r *. 4.), r /. 2.)) edges

(* camera *)
(*let camloc  = ( r*.4.,  r*.6., r*.7.)*)
let camloc = (14., 6., 8.)
let camdir  = zv -^ dir camloc

let spheres = ((camloc /^ 2.) +^ (-1.5,1.,0.5), r /. 2.)::spheres

let r () = Random.float 1.0

(* lights *)
let lights = make_list 1(*00*) (fun i ->
  point_light (r()*.20.-.10.,15.,r()*.20.-.10.) (white -^ (r(),r(),r()) *^ 0.2) 10.0
)

(* objects *)
let ground_obj = make_obj (make_plane ground) dull_white None
let wall1_obj = make_obj (make_plane wall1) mirror None
let wall2_obj = make_obj (make_plane wall2) mirror None

let [lens;s1;s2;s3;s4] = spheres

let sphere_objs = [
  make_obj (make_sphere lens) mirror (Some clear_glass);
  make_obj (make_sphere s1) shiny_red None;
  make_obj (make_sphere s2) shiny_green None;
  make_obj (make_sphere s3) shiny_blue None;
  make_obj (make_sphere s4) shiny_white None;
]

let wall_objs = [wall1_obj; wall2_obj]

let cyl_objs = List.map (fun c -> make_obj (make_cyl c false) shiny_blue None) cyls
let cube_sphere_objs = List.map (fun s -> make_obj (make_sphere s) shiny_blue None) cube_spheres

let mirror_sphere_obj = make_obj (make_sphere ((0.,0.,0.), 3.5)) mirror None

(* render params *)
let width = 600 (* 1680 *)
let height = 600 (* 1050 *)
let jitter = 0.2
let numrays = 5
let aspect = (float width) /. (float height)
let fovy = 2. *. (atan 1.)
let fovx = 2. *. (atan aspect)

(* scene *)
let cam = (camloc, dir camdir, (0.,1.,0.), fovx, fovy)
let scene = cam, [ground_obj; mirror_sphere_obj] @ cyl_objs @ cube_sphere_objs (*@ wall_objs*) (*@ sphere_objs*) @ lights

;;

seed();
draw complexity_view width height (draw_scene complexity_view jitter numrays scene);
ignore (wait_next_event [Key_pressed])
