open Types
open Math
open Draw
open Graphics
open Int
open Shaders
open Trace

let red = diffuse (1.,0.,0.)
let white = diffuse (1.,1.,1.)

let sphere = ((0.,1.,0.),1.)
let vol1 = sphere_vol sphere
let surf1 = red (int_sphere sphere)
let phys = (false, 0., 0.)
let sphere_obj = (vol1, surf1, phys)

let vol2 = plane_vol ground
let surf2 = white (int_plane ground)
let plane_obj = (vol2, surf2, phys)

let cam = ((0.,0.,0.), (0.,0.,1.), up, 90.0, 90.0)
let scene = [sphere_obj; plane_obj], [], cam

;;

seed();
draw 500 500 (draw_scene scene);
ignore (wait_next_event [Key_pressed])
