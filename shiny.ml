open Graphics
open Types
open Consts
open Math
open Draw
open Int
open Shaders
open Trace

(* helpers *)
let shiny diffuse specular = phong (diffuse, 0.8) (specular, 0.2) 10.
let dull diffuse = lambert (diffuse, 1.0)

let reflective reflect =        ((black, 0.), (black, 0.), reflect)
let emissive energy reflect =   (energy,      (black, 0.), reflect)
let absorptive energy reflect = ((black, 0.), energy,      reflect)

let make_plane plane = (plane_vol plane, int_plane plane)
let make_sphere sphere = (sphere_vol sphere, int_sphere sphere)
let make_obj shape surface substance = Object (shape, surface, substance)

(* shapes *)
let ground  = ( 0.0,  1.0,  0.0),  0.0

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

(* camera *)
let camloc  = ( 0.0,  r*.2., r*.5.)
let camdir  = ( 0.0,  0.0, -1.0)

(* lights *)
let lights = [
  point_light (-10.0, 10.0, 0.0) white 1.0;
  point_light (5.0, -10.0, 5.0) white 1.0
]

(* surfaces *)
let shiny_red = ((red,0.3), (white,0.5), (shiny red white))
(*let dull_white = absorptive (white, 0.8) (dull white)*)
(*let dull_red = reflective (dull red)*)
(*let shiny_red_glowy = emissive (blue, 0.2) (shiny red white)*)
let glowy = emissive (white, 1.0) (dull black)

(* objects *)
let plane_obj = make_obj (make_plane ground) glowy None
let sphere_objs = List.map (fun s -> make_obj (make_sphere s) shiny_red None) spheres

(* scene *)
let cam = (camloc, dir camdir, (0.,1.,0.), d2r 106., d2r 90.)
let scene = cam, [plane_obj] @ sphere_objs @ lights

;;

seed();
draw 1024 768 (draw_scene scene);
ignore (wait_next_event [Key_pressed])
