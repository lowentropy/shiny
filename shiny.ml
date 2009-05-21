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
let make_obj shape surf mat phys = Object (shape,surf,mat,phys)

(* shapes *)
let plane1 = (0.0, 1.0, 0.0), 4.4
let sphere1 = (1.0, -0.8, 3.0), 2.5
let sphere2 = (-5.5, -0.5, 7.0), 2.0

(* lights *)
let lights =
	let rec gen i =
		if i > 99 then [] else
		let x = (i mod 10) * 2 - 10 in
		let z = (i / 10) * 2 - 10 in
		let light = point_light (float x, 10.0, float z) (gray 0.05) in
		light::(gen (i+1)) in
	gen 0

(* surfaces *)
let p1_surf = (0.8, 0.2, 50.0, 0.0, 0.0)
let s1_surf = (0.6, 0.4, 10.0, 0.0, 0.0)
let s2_surf = (0.1, 0.8, 30.0, 0.5, 0.0)

(* materials *)
let p1_mat = (black, gray 0.6, white)
let s1_mat = (black, red, white)
let s2_mat = (black, (0.6,0.7,0.8), white)

(* objects *)
let p1_obj = make_obj (make_plane plane1)   p1_surf p1_mat None
let s1_obj = make_obj (make_sphere sphere1) s1_surf s1_mat None
let s2_obj = make_obj (make_sphere sphere2) s2_surf s2_mat None

(* scene *)
let cam = ((0.,0.,-10.), (dir (0.,0.,1.)), (0.,1.,0.), d2r 90., d2r 90.)
let scene = cam, ([p1_obj; s1_obj; s2_obj] @ lights)

;;

seed();
draw 500 500 (draw_scene scene);
ignore (wait_next_event [Key_pressed])
