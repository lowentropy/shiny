open Types
open Math
open Draw
open Graphics
open Int
open Shaders
open Trace

let red = diffuse (1.,0.,0.)
let blue = diffuse (0.,0.,1.)
let gray = diffuse (0.7,0.7,0.7)

let shiny = phong (0.,0.,0.5) (0.5,0.5,0.5) 30.0

let sphere = ((0.,1.0,0.),1.)
let vol1 = sphere_vol sphere
let surf1 = shiny, int_sphere sphere
let phys = (0.0, false, zv)
let sphere_obj = (vol1, surf1, phys)

let vol2 = plane_vol ground
let surf2 = gray, int_plane ground
let plane_obj = (vol2, surf2, phys)

let cam = ((0.,1.,3.), (dir (0.,0.,-1.)), (0.,1.,0.), d2r 90., d2r 90.)
let light1 = (-5.,7.,10.), (1.,1.,1.)
let light2 = ( 3.,1.,5.), (1.,1.,1.)
let scene = [sphere_obj; plane_obj], [light1; light2], cam

;;

seed();
draw 500 500 (draw_scene scene);
ignore (wait_next_event [Key_pressed])
