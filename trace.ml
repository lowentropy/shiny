open Types
open Math

let shoot cam x y =
	let c, d, u, fx, fy = cam in
	let r = d ^^ u in
	let ax = (x -. 0.5) *. fx in
	let ay = (0.5 -. y) *. fy in
	let v = d +^ r *^ (sin ax) +^ u *^ (sin ay) in
	(c, dir v)

let trace ray objects lights importance =
	(0.,0.,0.)

let draw_scene scene w h x y =
	let objects, lights, camera = scene in
	let px = (float x) /. (float (w - 1)) in
	let py = (float y) /. (float (h - 1)) in
	let eye = shoot camera px py in
	trace eye objects lights 1.0
