open Types
open Consts

(* basic randomization *)
let seed () = Random.self_init ()

(* tolerance-based floating-points *)
let ftol = 0.00000001
let fabs f = if f < 0. then -.f else f
let fzero f = (fabs f) < ftol
let fneg f = not (f > 0. || (fzero f))
let fpos f = not (f < 0. || (fzero f))
let fmin a b = if a < b then a else b
let fmax a b = if a > b then a else b

(* vector mapping *)
let vmap (x,y,z) f = (f x, f y, f z)
let vmap2 (a,b,c) (d,e,g) f = (f a d, f b e, f c g)
let vtrace (x,y,z) = x +. y +. z

(* vector member access *)
let vx (x,_,_) = x
let vy (_,y,_) = y
let vz (_,_,z) = z
let vi (x,y,z) i = match i with
	0 -> x | 1 -> y | 2 -> z |
	_ -> raise IllegalAxis

(* matrix mapping *)
let mmap m f = vmap m (fun v -> vmap v f)
let mmap2 a b f = vmap2 a b (fun v w -> vmap2 v w f)

(* matrix member access *)
let r1 m = vx m
let r2 m = vy m
let r3 m = vz m
let c1 m = vmap m vx
let c2 m = vmap m vy
let c3 m = vmap m vz

(* basic vector math *)
let ( +^ ) a b = vmap2 a b (+.)
let ( -^ ) a b = vmap2 a b (-.)
let ( *^ ) v s = vmap v (( *.) s)
let ( /^ ) v s = v *^ (1. /. s)
let dot a b = vtrace (vmap2 a b ( *.))
let dotp a b = max 0.0 (dot a b)
let ( ^^ ) (a,b,c) (d,e,f) =
	(b *. f -. c *. e,
	 c *. d -. a *. f,
	 a *. e -. b *. d)
let stp a b c = dot a (b ^^ c)
let ( <^ ) (x1,y1,z1) (x2,y2,z2) =
	(x1 < x2) || (y1 < y2) || (z1 < z2)
let ( >^ ) (x1,y1,z1) (x2,y2,z2) =
	(x1 > x2) || (y1 > y2) || (z1 > z2)

(* basic matrix math *)
let ( +| ) a b = mmap2 a b (+.)
let ( -| ) a b = mmap2 a b (-.)
let ( *| ) m s = mmap m (( *.) s)
let ( /| ) m s = m *| (1. /. s)
let tr m = (c1 m, c2 m, c3 m)
let mdiag m = (vx (r1 m), vy (r2 m), vz (r3 m))
let mtrace m = vtrace (mdiag m)

(* vector/matrix math *)
let mv m v = vmap m (dot v)
let vm v m = vmap (tr m) (dot v)
let mm a b = vmap a (fun r -> vmap (tr b) (dot r))

(* matrix determinant *)
let det m =
	let ((aa,ab,ac),(ba,bb,bc),(ca,cb,cc)) = m in
	aa *. (bb *. cc -. bc *. cb) -.
	ab *. (ba *. cc -. bc *. ca) +.
	ac *. (ba *. cb -. bb *. ca)
let singular m = fzero (det m)

(* more vector math *)
let mag v = sqrt (dot v v)
let dir v = 
	let m = mag v in
	if fzero m then v else v /^ m

(* reflection about normal: v towards surface *)
let reflect v n = v -^ n *^ (2. *. (dot v n))

(* miscellaneous *)
let vsum lst = List.fold_left (+^) zv lst
let ray_at (o,d) t = o +^ d *^ t
let lift o d = (ray_at (o,d) ftol), d
let pi = 3.14159265358979323
let twopi = pi *. 2.
let d2r d = d *. pi /. 180.
let r2d r = r *. 180. /. pi
let combine a b = vmap2 a b ( *. )

let rec make_list n f =
  let i = n - 1 in if i < 0 then [] else (f i)::(make_list i f)
  
let quadratic a b c =
  let t = b *. b -. 4. *. a *. c in
  if t < 0. then None else
  let t = sqrt t in
  let a2 = a *. 2. in
  let r1 = (-.b -. t) /. a2 in
  let r2 = (-.b +. t) /. a2 in
  Some (r1, r2)