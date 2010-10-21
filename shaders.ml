open Types
open Consts
open Math
open Format

let phong diffuse specular alpha l n e =
  let (d, kd), (s, ks) = diffuse, specular in
  let d = d *^ kd in
  let s = if ks > 0. then
    let h = dir (e -^ l) in
    let a1 = dot n h in
    let a2 = dot n (zv -^ l) in
    if a1 < 0. || a2 < 0. then zv else
    s *^ (ks *. (a1 ** alpha) /. a2)
    else zv in
  (d +^ s) *^ (dotp n (zv -^ l))

let lambert diffuse l n (e:vec) =
  let (d, kd) = diffuse in
  d *^ (kd *. (dotp n (zv -^ l)))

let point_light pos color factor = Light (ghost, (fun n -> [(pos,None)]), (color, factor))

let area_light center axis1 axis2 color factor =
  let norm = Some (dir (axis1 ^^ axis2)) in
  Light (ghost, (fun n ->
    make_list n (fun i ->
      let p = center
                +^ (axis1 *^ ((Random.float 2.) -. 1.))
                +^ (axis2 *^ ((Random.float 2.) -. 1.)) in
      (p, norm)
    )
  ), (color, factor))

let fresnel_coeff l n r n1 n2 =
  let cl, cr = -.(dot l n), -.(dot r n) in
  let n1cl, n1cr = n1 *. cl, n1 *. cr in
  let n2cl, n2cr = n2 *. cl, n2 *. cr in
  let rs = (n1cl -. n2cr) /. (n1cl +. n2cr) in
  let rp = (n2cl -. n1cr) /. (n2cl +. n1cr) in
  let kr1 = (rs *. rs +. rp *. rp) /. 2. in
  let kr2 = 1. -. kr1 in
  (kr1, kr2)