open Types
open Consts
open Math

let phong diffuse specular alpha l n e =
  let (d, kd), (s, ks) = diffuse, specular in
  let d = if kd > 0. then
    let a = dotp (zv -^ l) n in
    d *^ (kd *. a)
    else zv in
  let s = if ks > 0. then
    let r = reflect l n in
    let a = dotp r e in
    s *^ (ks *. (a ** alpha))
    else zv in
  d +^ s

let lambert diffuse l n e =
  let (d, kd) = diffuse in
  let a = dotp l n in
  d *^ (a *. kd)

let point_light pos color factor = Light (ghost, (fun n -> [pos]), (color, factor))

let fresnel_coeff l n r n1 n2 =
  let cl, cr = -.(dot l n), dot r n in
  let n1cl, n1cr = n1 *. cl, n1 *. cr in
  let n2cl, n2cr = n2 *. cl, n2 *. cr in
  let rs = (n1cl -. n2cr) /. (n1cl +. n2cr) in
  let rp = (n1cr -. n2cl) /. (n1cr +. n2cl) in
  let kr1 = (rs +. rp) /. 2. in
  let kr2 = 1. -. kr1 in
  (kr1, kr2)