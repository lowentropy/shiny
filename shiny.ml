open Graphics
open Types
open Consts
open Math
open Draw
open Int
open Shaders
open Trace

(* helpers *)

let rec make_list n f =
	if n = 0 then [] else
	(f (n-1))::(make_list (n-1) f)

let make_abplane abplane = (abplane_vol abplane, int_abplane abplane)
let make_plane plane = (plane_vol plane, int_plane plane)
let make_obj shape surf mat phys = Object (shape,surf,mat,phys)

(* shapes *)
let plane1  = ( 0.0,  1.0,  0.0),  0.0
let camloc  = ( 0.0,  4.0, 15.0)
let camdir  = ( 0.0,  0.0, -1.0)

let ymin, ymax = 5.0, 6.0

let slats1  = make_list 10 (fun i ->
	let x = (float i) -. 4.5 in
	let p = ((1.0, 0.0, 0.0), -.x) in
	(p, (x-.0.1,ymin,-4.5), (x+.0.1,ymax,4.5)))

let slats2  = make_list 10 (fun i ->
	let z = (float i) -. 4.5 in
	let p = ((0.0, 0.0, 1.0), -.z) in
	(p, (-4.5,ymin,z-.0.1), (4.5,ymax,z+.0.1)))

let lights = make_list 16 (fun i ->
	let x = (float (i/4)) -. 1.6 in
	let z = (float (i mod 4)) +. 0.3 in
	let c = match (i mod 3) with
		0 -> (0.3, 0.0, 0.0)
	  | 1 -> (0.0, 0.3, 0.0)
	  | _ -> (0.0, 0.0, 0.3) in
	point_light (x, 15.0, z) c)

(* surfaces *)
let the_surf = (0.7 , 0.0,  0.0, 0.0, 0.0)

(* materials *)
let the_mat = (black, white, white)

(* objects *)
let plane_obj = make_obj (make_plane plane1) the_surf the_mat None
let slat_objs = List.map (fun s -> make_obj (make_abplane s) the_surf the_mat None)
	(slats1 @ slats2)

(* scene *)
let cam = (camloc, dir camdir, (0.,1.,0.), d2r 90., d2r 90.)
let scene = cam, (plane_obj::(lights @ slat_objs))

;;

seed();
draw 500 500 (draw_scene scene);
ignore (wait_next_event [Key_pressed])
