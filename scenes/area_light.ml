open Graphics
open Types
open Math
open Draw
open Int
open Trace
open Consts
open Shaders
open Materials
open Builders

let complexity_view = false

let radius     = 1.0
let ground     = (0.0,  1.0,  0.0),  0.0
let camloc     = (0., 5., 5.)
let sphere     = ((0.0, radius, 0.0), radius)
let camdir     = zv -^ dir camloc
let light_size = 10.0
let light_y    = 8.0
let light_str  = 80.0

let objects = [
  make_obj (make_sphere sphere) shiny_red   None;
  make_obj (make_plane ground)  dull_white  None;
]

let lights = [
  area_light (0., light_y, 0.0) (light_size, 0.0, 0.0) (0.0, 0.0, light_size) white light_str;
]

(* render params *)
let width = 400 (* 1680 *)
let height = 400 (* 1050 *)
let jitter = 0.2
let numrays = 1
let aspect = (float width) /. (float height)
let fovy = 2. *. (atan 1.)
let fovx = 2. *. (atan aspect)

(* scene *)
let cam = (camloc, dir camdir, (0.,1.,0.), fovx, fovy)
let scene = cam, objects @ lights

;;

seed();
draw complexity_view width height (draw_scene complexity_view jitter numrays scene);
print_string "Done drawing!\n";
ignore (wait_next_event [Key_pressed])
