open Types
open Math
open Consts
open Shaders
open Format
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

let intersect ray entities =
	List.fold_left (fun near ent ->
		let shape = shape_of ent in
		let bound, intersect = shape in
		if not (bound ray) then near else
		match near, intersect ray with
			_, None -> near
		  | None, Some (t,n,p) -> Some (ent, (t,n,p))
		  | Some (_,(u,_,_)), Some (t,n,p) ->
		  		if u < t then near else Some (ent, (t,n,p))
		) None entities

let hits_before ray entities dist =
	List.exists (fun ent ->
		let shape = shape_of ent in
		let bound, intersect = shape in
		if not (bound ray) then false else
		match intersect ray with
			None -> false
		  | Some (t,_,_) -> t < dist
		) entities

let lights entities =
	List.fold_left (fun lights ent ->
		match ent with
			Light l -> l::lights
		  | _ -> lights) [] entities

let jitter_ray (o, d) j =
	let x, y, z = d in
	let r1 = (z, y, -.x) in
	let r2 = d ^^ r1 in
	let xo = (Random.float j) *. 2. -. j in
	let yo = (Random.float j) *. 2. -. j in
	(o, dir (d +^ (r1 *^ xo) +^ (r2 *^ yo)))

let direct_light p n e entities samples surface material =
	let (ambient, _, _) = material in
	List.fold_left (fun sum (_,sample,color) ->
		let samples = sample samples in
		let num = List.length samples in
		let sampled = List.fold_left (fun total point ->
			let diff = point -^ p in
			let dist = mag diff in
			let dir = diff /^ dist in
			let ray = jitter_ray (lift p dir) 0.001 in
			if hits_before ray entities dist then black else
			let mult = phong surface material n e dir in
			total +^ (combine color mult)
		) black samples in
		sum +^ (sampled /^ (float num))
	) ambient (lights entities)

let rec trace ray entities n1 importance =
	match intersect ray entities with
		None -> zv | Some (ent, (t,n,p)) ->
	let o, d = ray in
	match ent with
		Light (_, _, color) -> color
	  | Object (_, surface, material, physics) ->
	let kd, ks, _, kr1, kr2 = surface in
	let direct = if ((kd > 0.) || (ks > 0.))
		then direct_light p n (zv-^d) entities 9 surface material
		else black in
	let refracted = (
		match physics with
			None -> black
		  |	Some (n2, absorb) ->
		let importance = kr2 *. importance in
		if importance < 0.2 then black else
		black (* TODO: refraction*)) in
	let reflected = (
		let importance = kr1 *. importance in
		if importance < 0.2 then black else
		let ray = lift p (reflect d n) in
		let color = trace ray entities n1 (importance *. kr1) in
		color *^ kr1) in
	direct +^ refracted +^ reflected

let jitter x j = x +. Random.float (j *. 2.) -. j

let draw_scene ?(j=0.0) scene w h x y =
	let camera, entities = scene in
	let px = (jitter (float x) j) /. (float (w - 1)) in
	let py = (jitter (float y) j) /. (float (h - 1)) in
	let eye = shoot camera px py in
	trace eye entities 1.0 1.0
