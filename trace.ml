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
		if (not (bound ray)) then near else
		match near, intersect true ray with
			_, None -> near
		  | None, Some (t,n,p) -> Some (ent, (t,n,p))
		  | Some (_,(u,_,_)), Some (t,n,p) ->
		  		if u < t then near else Some (ent, (t,n,p))
		) None entities

let hits_before ray entities dist =
	List.exists (fun ent ->
		let shape = shape_of ent in
		let bound, intersect = shape in
		if (not (bound ray)) then false else
		match intersect true ray with
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

let direct_light p n e entities samples reflector =
  List.fold_left (fun sum (_,sampler,(color,factor)) ->
    let color = color *^ factor in
    let samples = sampler samples in
    let num = List.length samples in
    let sampled = List.fold_left (fun total (point, norm) ->
      let diff = point -^ p in
      let dist = mag diff in
      let dir = diff /^ dist in
      let ray = jitter_ray (lift p dir) 0.0 in
      if hits_before ray entities dist then black else
      let mult = reflector (zv -^ dir) n e in
      let cf = match norm with None -> 1.0 | Some n -> dotp n (zv -^ dir) in
      total +^ ((combine color mult) *^ (cf /. (dist *. dist)))
    ) black samples in
    sum +^ (sampled /^ (float num))
  ) zv (lights entities)

let refract p n e n1 n2 =
  let c1 = -.(dot n e) in
  let r = n1 /. n2 in
  let s = 1. -. r *. r *. (1. -. c1 *. c1) in
  if s < 0. then None else
  let c2 = sqrt s in
  let c2 = if c1 > 0. then -. c2 else c2 in
  let d = (e *^ r) +^ (n *^ (r *. c1 +. c2)) in
  Some (lift p d)

let min_importance = 0.2
let max_depth = 50
let area_light_samples = 100


let rec trace ray entities n1 importance depth =
  
  if depth > max_depth then (zv, 0) else

  (* intersect the ray with the scene*)
  match intersect ray entities with
    None -> (zv, 1) | Some (ent, (t,n,p)) ->

  (* if the ray hits a light, just use the light color *)
  let o, d = ray in
  match ent with
      Light (_, _, (color,_)) -> (color, 1)
    | Object (shape, surface, substance) ->
  let emission, absorption, reflection = surface in

  (* get the contribution from direct light sources *)
  let direct =
    direct_light p n (zv -^ d) entities area_light_samples reflection in

  (* refract light into the object*)
  let (refracted, kr2, max_depth) = (

    (* if the object doesn't have substance, it's internally black *)
    match substance with
        None -> (black, 0.0, 0)
      |	Some ((abs_color, abs_index), n2) ->

    (* refract from outer -> inner *)
    match refract p n d n1 n2 with
        None -> (black, 0.0, 0) (* total internal reflection *)
      | Some (o2, d2) ->
    let kr1, kr2 = fresnel_coeff d n d2 n1 n2 in
    
    (* black if unimportant *)
    let importance = kr2 *. importance in
    if importance < min_importance then (black, kr2, 0) else

    (* find ray exit point *)
    let _, intersect = shape in
    match intersect false (o2, d2) with
        None -> raise FunkySolid
      | Some (t, n, p2) ->

    (* refract from inner -> outer *)
    match refract p2 (zv -^ n) d2 n2 n1 with
        None -> (black, 0.0, 0) (* total internal reflection *)
      | Some ray ->

    (* find next intersection *)
    let (color, max_depth) = trace ray entities n1 importance (depth + 1) in
    let mult = vmap abs_color (fun x -> exp (-. t *. (x *. abs_index)) *. kr2) in
    ((combine color mult), kr2, max_depth)) in

  (* reflected color *)
  let (reflected, max_depth2) = (
    let kr1 = 1. -. kr2 in
    let l = reflect d n in
    let factor = reflection (zv -^ l) n (zv -^ d) in
    let importance = kr1 *. (mag factor) *. importance in
    if importance < min_importance then (black, 0) else
    let ray = lift p l in
    let (color, max_depth) = trace ray entities n1 importance (depth + 1) in
    ((combine color factor) *^ kr1), max_depth) in
  
  let max_depth = max_depth + max_depth2 + 1 in
  
  (* emitted color *)
  let (em_color, em_factor) = emission in
  let angle = dotp n (zv -^ d) in
  let emitted = em_color *^ (em_factor *. angle) in

  (* combine then absorb some *)
  let (abs_color, abs_factor) = absorption in
  let incoming = direct +^ refracted +^ reflected in
  let release = white -^ (abs_color *^ abs_factor) in
  let outgoing = emitted +^ (combine release incoming) in
  (outgoing, max_depth)

let jitter x j = x +. Random.float (j *. 2.) -. j

let rec make_list n f =
  let i = n - 1 in if i < 0 then [] else (f i)::(make_list i f)

let draw_scene depth j n scene w h x y =
  let camera, entities = scene in
  let w = w * n in
  let h = h * n in
  let j = j /. (float n) in
  let results = make_list (n*n) (fun i ->
    let b = i / n in
    let a = i mod n in
    let px = (jitter (float (x * n + a)) j) /. (float (w - 1)) in
    let py = (jitter (float (y * n + b)) j) /. (float (h - 1)) in
    let eye = shoot camera px py in
    trace eye entities 1.0 1.0 0
  ) in
  let colors = List.map (fun (c,_) -> c) results in
  let depths = List.map (fun (_,d) -> d) results in
  let total_depth = float (List.fold_left (fun i j -> i + j) 0 depths) in
  if depth
    then (total_depth, total_depth, total_depth)
    else List.fold_left (fun s c -> s +^ (c /^ (float (n*n)))) zv colors
