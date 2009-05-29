open Graphics
open Types
open Consts
open Math
open Draw
open Int
open Shaders
open Trace

(* helpers *)
let make_sphere sphere = (sphere_vol sphere, int_sphere sphere)
let make_plane plane = (plane_vol plane, int_plane plane)
let make_lens lens = (lens_vol lens, int_lens lens)
let make_obj shape surf mat phys = Object (shape,surf,mat,phys)

(* shapes *)
let plane1  = ( 0.0,  1.0,  0.0),  0.0
let plane2  = ( 0.0,  0.0,  1.0),  6.0
let plane3  = ( 1.0,  0.0,  0.0),  6.0
let plane4  = (-1.0,  0.0,  0.0),  6.0

let sphere1 = (-3.0,  4.0,  0.0),  2.0
let sphere2 = ( 3.0,  4.0,  0.0),  2.0

let lens1   = (0.5,  3.5,  8.0),  (dir (0.0, -0.5, 1.0)),  15.0,  0.1

let camloc  = ( 0.0,  4.0, 15.0)
let camdir  = ( 0.0,  0.0, -1.0)

(* lights *)
(* let light = point_light (0.0, 10.0, 0.0) (gray 2.0) *)

let rec n_of i f =
	if i = 0 then [] else
	(f ())::(n_of (i-1) f)

let light = Light (
	ghost,
	(fun n -> n_of n (fun () ->
		(Random.float 4.0 -. 2., 10.0, Random.float 4.0 -. 2.))),
	(gray 2.0))

(* surfaces *)
let p1_surf = (0.6 , 0.0,  0.0, 0.0, 0.0)
let s1_surf = (0.8 , 0.2, 10.0, 0.0, 0.0)
let s2_surf = (0.0,  0.5, 50.0, 0.8, 0.0)
let lens_surf = (0.0, 0.5, 50.0, 0.0, 0.8)

(* materials *)
let p1_mat = (black, gray 0.6, white)
let s1_mat = (black, red, white)
let s2_mat = (black, white, white)
let lens_mat = (black, white, white)

(* physics *)
let lens_phys = (1.5, (0.3,0.3,0.1) *^ 70.0)

(* objects *)
let p1_obj = make_obj (make_plane plane1)   p1_surf p1_mat None
let p2_obj = make_obj (make_plane plane2)   p1_surf p1_mat None
let p3_obj = make_obj (make_plane plane3)   p1_surf p1_mat None
let p4_obj = make_obj (make_plane plane4)   p1_surf p1_mat None
let s1_obj = make_obj (make_sphere sphere1) s1_surf s1_mat None
let s2_obj = make_obj (make_sphere sphere2) s2_surf s2_mat None
let lens_obj = make_obj (make_lens lens1) lens_surf lens_mat (Some lens_phys)

(* scene *)
let cam = (camloc, dir camdir, (0.,1.,0.), d2r 90., d2r 90.)
let scene = cam, [
	p1_obj;
	(* p2_obj; p3_obj; p4_obj; *)
	s1_obj; s2_obj;
	lens_obj; 
	light
]

;;

seed();
draw 500 500 (draw_scene scene);
ignore (wait_next_event [Key_pressed])
