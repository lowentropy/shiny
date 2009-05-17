open Types
open Math
open Pp

let shoot cam x y =
	let c, d, u, fx, fy = cam in
	let sx = tan (fx /. 2.) in
	let sy = tan (fy /. 2.) in
	let r = dir (d ^^ u) in
	let u = r ^^ d in
	let dx = (x -. 0.5) *. sx  in
	let dy = (0.5 -. y) *. sy in
	let v = d +^ (r *^ dx) +^ (u *^ dy) in
	(c, dir v)

let refl_of (obj:obj) =
	let _, (refl, _), _ = obj in refl

let intersect ray objects =
	List.fold_left (fun near obj ->
		let vol, (_,ifun), _ = obj in
		if not (vol ray) then near else
		match near, ifun ray with
			_, None -> near
		  | None, Some (t,n,p) -> Some (obj, (t,n,p))
		  | Some (_,(u,_,_)), Some (t,n,p) ->
		  		if u < t then near else Some (obj, (t,n,p))
		) None objects

let hits_before ray objects dist =
	List.exists (fun obj ->
		let _, (_,ifun), _ = obj in
		match ifun ray with
			None -> false
		  | Some (t,_,_) -> t < dist
		) objects

let trace (o,d) objects lights importance =
	match intersect (o,d) objects with
		None -> zv | Some (obj, (t,n,p)) ->
	let refl = (refl_of obj) (t,n,p) d in
	let direct = vsum (List.map (fun (loc,color) ->
		let s = loc -^ p in
		let l = dir s in
		if hits_before (lift p l) objects (mag s) then zv else
		vmap2 color (refl l) ( *. )) lights) in
	direct

let draw_scene scene w h x y =
	let objects, lights, camera = scene in
	let px = (float x) /. (float (w - 1)) in
	let py = (float y) /. (float (h - 1)) in
	let eye = shoot camera px py in
	trace eye objects lights 1.0
