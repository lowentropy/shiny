open Types
open Math

let plane_vol (n,d) (o,r) =
	let v = dot n r in
	if fzero v then false else
	let t = -. (d +. (dot n o)) /. v in
	if t < 0. then false else true

(* ray-plane intersection *)
let int_plane (n,d) cull (o,r) =
	let v = dot n r in
	if fzero v then None else
	if cull && v > 0. then None else
	let t = -. (d +. (dot n o)) /. v in
	if t < 0. then None else
	Some (t, n, ray_at (o,r) t)

(* sphere bounding-volume *)
let sphere_vol (c,rad) (o,d) =
	let o = o -^ c in
	let ll = dot o o in
	let rr = rad *. rad in
	if ll < rr then true else
	let tca = -. (dot o d) in
	if (fneg tca) then false else
	let t2hc = rr -. ll +. (tca *. tca) in
	if (not (fpos t2hc)) then false
	else true

(* ray-sphere intersection *)
let int_sphere (c,rad) cull (o,d) =
	(* find closest-apprach distance *)
	let o = o -^ c in
	let ll = dot o o in
	let rr = rad *. rad in
	let i = ll < rr in
	if i && cull then None else
	(* if pointing away, ray misses *)
	let tca = -. (dot o d) in
	if not i && (fneg tca) then None else
	(* if dist > radius, ray misses *)
	let t2hc = rr -. ll +. (tca *. tca) in
	if not i && (not (fpos t2hc)) then None else
	(* find both intersection points *)
	let thc = sqrt t2hc in
	let t1 = tca -. thc in
	let t2 = tca +. thc in
	(* find nearest point and surface normal *)
	let t = if t1 < 0. then t2 else t1 in
	let p = ray_at (o,d) t in
	let n = p /^ rad in
	Some (t, n, p +^ c)

let lens_vol (c,n,r,l) (o,d) = sphere_vol (c,r) (o,d)

let int_lens (c,n,r,l) cull (o,d) =
	let rr = r *. r in
	let f = n *^ (r -. l) in
	let cr = c +^ f in
	let cl = c -^ f in
	let sr = cr, r in
	let sl = cl, r in
	let dr = o -^ cr in
	let dl = o -^ cl in
	let drdr = dot dr dr in
	let dldl = dot dl dl in
	if (dldl < rr) && (drdr < rr) then (
		if cull then None else
		let s = if (dot n d) > 0. then sl else sr in
		int_sphere s false (o,d)) else
	let s = if (dot (o -^ c) n) > 0. then sl else sr in
	int_sphere s true (o,d)
