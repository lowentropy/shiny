open Types
open Math

let shoot cam x y =
	let c, d, u, fx, fy = cam in
	let r = d ^^ u in
	let ax = (x -. 0.5) *. fx in
	let ay = (0.5 -. y) *. fy in
	let v = d +^ r *^ (sin ax) +^ u *^ (sin ay) in
	(c, dir v)

let refl_of (obj:obj) =
	let _, (refl, _), _ = obj in refl

let intersect (o,d) objects =
	None

let trace (o,d) objects lights importance =
	match intersect (o,d) objects with
		None -> zv | Some (obj, (t,n,p)) ->
	let refl = (refl_of obj) (t,n,p) d in
	let direct = vsum (List.map (fun (loc,color) ->
		let l = dir (loc -^ p) in
		vmap2 color (refl l) ( *. )) lights) in
	direct

let draw_scene scene w h x y =
	let objects, lights, camera = scene in
	let px = (float x) /. (float (w - 1)) in
	let py = (float y) /. (float (h - 1)) in
	let eye = shoot camera px py in
	trace eye objects lights 1.0
