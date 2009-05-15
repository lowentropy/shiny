open Types
open Math

(* ray-sphere intersection *)
let int_sphere ?(cull=true) rad ray =
	let o, d = ray in
	(* find closest-apprach distance *)
	let l = mag o in
	let i = l < rad in
	if i && cull then None else
	(* if pointing away, ray misses *)
	let tca = -. (dot o d) in
	if not i && (fneg tca) then None else
	(* if dist > radius, ray misses *)
	let t2hc = (rad ** 2.) -. (l ** 2.) +. (tca ** 2.) in
	if not i && (not (fpos t2hc)) then None else
	(* find both intersection points *)
	let thc = sqrt t2hc in
	let t1 = tca +. thc in
	let t2 = tca -. thc in
	(* find nearest positive point and return *)
	let t = (if t1 < 0. then t2 else t1) in
	Some t

(* ray-triangle intersection *)
let int_tri ?(cull=true) v0 v1 v2 n ray =
	let o, d = ray in
	(* get edge vectors and normalized distance *)
	let e1, e2 = v1 -^ v0, v2 -^ v0 in
	let l = stp e1 d e2 in
	if fzero l || (cull && l < 0.) then None else
	(* get u coordinate *)
	let r = o -^ v0 in
	let u = stp r d e2 in
	if u < 0. || u > l then None else
	(* get v coordinate *)
	let q = r ^^ e1 in
	let v = dot d q in
	if v < 0. || u +. v > l then None else
	(* get t and return *)
	let t = (dot e2 q) /. l in
	Some t
