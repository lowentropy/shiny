open Types
open Math

(* ray-plane intersection *)
let int_plane ?(cull=true) (n,d) (o,r) =
	let v = dot n r in
	if fzero v then None else
	if cull && v > 0. then None else
	let t = -. (d +. (dot n o)) /. v in
	if t < 0. then None else
	let p = o +^ r *^ t in
	Some (p, n)

(* sphere bounding-volume *)
let sphere_vol (c,rad) (o,d) =
	let o = o -^ c in
	let l = dot o o in
	let rr = rad *. rad in
	if l < rr then true else
	let tca = -. (dot o d) in
	if tca < 0. then false else
	let t2hc = rr -. l +. (tca *. tca) in
	if t2hc <= 0. then false
	else true

(* ray-sphere intersection *)
let int_sphere ?(cull=true) (c,rad) (o,d) =
	(* find closest-apprach distance *)
	let o = o -^ c in
	let l = dot o o in
	let rr = rad *. rad in
	let i = l < rr in
	if i && cull then None else
	(* if pointing away, ray misses *)
	let tca = -. (dot o d) in
	if not i && (fneg tca) then None else
	(* if dist > radius, ray misses *)
	let t2hc = rr -. l +. (tca *. tca) in
	if not i && (not (fpos t2hc)) then None else
	(* find both intersection points *)
	let thc = sqrt t2hc in
	let t1 = tca +. thc in
	let t2 = tca -. thc in
	(* find nearest point and surface normal *)
	let t = if t1 < 0. then t2 else t1 in
	let p = o +^ d *^ t in
	let n = p /^ rad in
	Some (p, n)
