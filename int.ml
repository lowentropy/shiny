open Types
open Math
open Pp

let plane_vol (n,d) (o,r) =
	let v = dot n r in
	if fzero v then false else
	let t = -. (d +. (dot n o)) /. v in
	t >= 0.

(* ray-plane intersection *)
let int_plane (n,d) cull (o,r) =
	let v = dot n r in
	if fzero v then None else
	if cull && v > 0. then None else
	let t = -. (d +. (dot n o)) /. v in
	if t < 0. then None else
	Some (t, n, ray_at (o,r) t)

(* axis-bounded plane *)
let abplane_vol (p,_,_) r = plane_vol p r

(* intersect axis-bounded plane *)
let int_abplane (plane,min,max) _ r =
	match int_plane plane false r with
		None -> None
	  | Some (t,n,p) ->
	if (p <^ min) || (p >^ max)
		then None
		else Some (t,n,p)

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
	if (i && cull) then None else
	(* if pointing away, ray misses *)
	let tca = -. (dot o d) in
	if ((not i) && (fneg tca)) then None else
	(* if dist > radius, ray misses *)
	let t2hc = rr -. ll +. (tca *. tca) in
	if ((not i) && (not (fpos t2hc))) then None else
	(* find both intersection points *)
	let thc = sqrt t2hc in
	let t1 = tca -. thc in
	let t2 = tca +. thc in
	(* find nearest point and surface normal *)
	let t = if t1 < 0. then t2 else t1 in
	let p = ray_at (o,d) t in
	let n = p /^ rad in
	Some (t, n, p +^ c)

(* fake a lens volume with its bounding sphere *)
let lens_vol (c,n,r,l) (o,d) = sphere_vol (c,r) (o,d)

(* R = (r^2 + (1/4) w^2) / w *)
let int_lens (c,n,r,w) cull (o,d) =
	let r = ((r*.r) +. ((w*.w) /. 4.)) /. w in
	let l = w /. 2. in
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
	let s1, q1, s2, q2 =
		if (dot (o -^ c) n) > 0.
			then (sl,cr,sr,cl)
			else (sr,cl,sl,cr) in
	let result = match int_sphere s1 true (o,d) with
		None -> (
			match int_sphere s2 true (o,d) with
				None -> None
			  | Some (t,n,p) -> Some (q2,t,n,p))
	  | Some (t,n,p) -> Some (q1,t,n,p) in
	match result with
		None -> None
	  | Some (q,t,n,p) ->
	let qd = q -^ p in
	let qq = dot qd qd in
	if qq < rr then Some (t,n,p) else None
	
let cyl_vol (a,b,r) (o,d) =
  let ab = b -^ a in
  let ao = o -^ a in
  let aoab = ao ^^ ab in
  let dab = d ^^ ab in
  let ab2 = dot ab ab in
  let qa = dot dab dab in
  let qb = 2. *. (dot dab aoab) in
  let qc = (dot aoab aoab) -. (r *. r *. ab2) in
  match quadratic qa qb qc with
    None -> false | Some (t1,t2) ->
  let t = if t1 < 0. then t2 else if t1 < t2 then t1 else t2 in
  not (fneg t)
  
let int_cyl_caps a b r (o,d) cull =
  let n = dir (a -^ b) in
  let k = -. (dot n a) in
  match int_plane (n,k) cull (o,d) with
    None -> None |
    Some (t,n,p) ->
  let d = mag (p -^ a) in
  if d > r then None else Some (t,n,p)

let int_cyl (a,b,r) capped cull (o,d) =
  let ab = b -^ a in
  let ao = o -^ a in
  let aoab = ao ^^ ab in
  let dab = d ^^ ab in
  let ab2 = dot ab ab in
  let qa = dot dab dab in
  let qb = 2. *. (dot dab aoab) in
  let qc = (dot aoab aoab) -. (r *. r *. ab2) in
  match quadratic qa qb qc with
    None -> None | Some (t1,t2) ->
  let inside = t1 < 0. in
  let t = if inside then t2 else min t1 t2 in
  if inside && cull then None else
  if fneg t then None else
  let p = ray_at (o,d) t in
  let q = p -^ a in
  let q = (dot ab q) /. (dot ab ab) in
  if q < 0. then
    if capped then int_cyl_caps a b r (o,d) cull else None else
  if q > 1. then
    if capped then int_cyl_caps b a r (o,d) cull else None else
  let q = ab *^ q +^ a in
  let n = dir (p -^ q) in
  Some (t,n,p)