open Graphics
open Types
open Consts
open Math
open Draw
open Int
open Shaders
open Trace

(* helpers *)
let shiny diffuse specular = phong (diffuse, 0.5) (specular, 0.5) 1000.
let dull diffuse = lambert (diffuse, 0.5)

let reflective reflect =        ((black, 0.), (black, 0.), reflect)
let emissive energy reflect =   (energy,      (black, 0.), reflect)
let absorptive energy reflect = ((black, 0.), energy,      reflect)

let make_plane plane = (plane_vol plane, int_plane plane)
let make_sphere sphere = (sphere_vol sphere, int_sphere sphere)
let make_cyl cyl capped = (cyl_vol cyl, int_cyl cyl capped)
let make_obj shape surface substance = Object (shape, surface, substance)

(* shapes *)
let ground  = (0.0,  1.0,  0.0),  0.0
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

let cyl = ((-1.,0.5,0.),(1.,0.5,0.),0.5)

(* camera *)
(*let camloc  = ( r*.4.,  r*.6., r*.7.)*)
let camloc = (3., 3., 3.)
let camdir  = zv -^ dir camloc

let spheres = ((camloc /^ 2.) +^ (-1.5,1.,0.5), r /. 2.)::spheres

let r () = Random.float 1.0

(* lights *)
let lights = make_list 10 (fun i ->
  point_light (r()*.4.-.2.,5.,r()*.4.-.2.) (white -^ (r(),r(),r()) *^ 0.2) 10.0
)

(* surfaces *)
let shiny_red = reflective (shiny red white)
let shiny_green = reflective (shiny green white)
let shiny_blue = reflective (shiny blue white)
let shiny_white = reflective (shiny white white)

let dull_white = absorptive (white, 0.5) (dull white)
let dull_red = emissive (red, 1.0) (dull black)
let dull_green = absorptive (white, 0.5) (dull green)

let mirror = reflective (phong (black, 0.0) (white, 1.0) 1000.0)
let transparent = reflective (dull black)

(* substances *)
let blue_glass = ((white -^ blue, 0.5), 1.5)
let red_glass = ((white -^ red, 0.5), 1.5)
let green_glass = ((white -^ green, 0.5), 1.5)
let clear_glass = ((black, 0.0), 1.5)


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

let cyl_obj = make_obj (make_cyl cyl true) shiny_red None

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
let scene = cam, [ground_obj; cyl_obj] (*@ wall_objs*) (*@ sphere_objs*) @ lights

;;

seed();
draw width height (draw_scene jitter numrays scene);
ignore (wait_next_event [Key_pressed])
