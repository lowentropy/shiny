open Graphics
open Types
open Math
open Draw
open Int
open Shaders
open Trace

let hollow = (0.0, false, zv)

let plane1 = (0.0, 1.0, 0.0), 4.4
let sphere1 = (1.0, -0.8, 3.0), 2.5
let sphere2 = (-5.5, -0.5, 7.0), 2.0
let light1 = (0.0, 5.0, 5.0), (0.6, 0.6, 0.6)
let light2 = (2.0, 5.0, 1.0), (0.7, 0.7, 0.9)

let p1mat = diffuse (0.4, 0.3, 0.3)
let s1mat = phong 1.0 (0.7,0.7,0.7) 0.6 white 20.0
let s2mat = phong 0.1 (0.7,0.7,0.1) 1.0 white 20.0

let p1obj = (plane_vol plane1, (p1mat, int_plane plane1), hollow)
let s1obj = (sphere_vol sphere1, (s1mat, int_sphere sphere1), hollow)
let s2obj = (sphere_vol sphere2, (s2mat, int_sphere sphere2), hollow)

let cam = ((0.,0.,-5.), (dir (0.,0.,1.)), (0.,1.,0.), d2r 90., d2r 90.)

let scene = [p1obj; s1obj; s2obj], [light1; light2], cam

;;

seed();
draw 500 500 (draw_scene scene);
ignore (wait_next_event [Key_pressed])
