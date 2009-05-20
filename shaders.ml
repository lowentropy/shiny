open Types
open Consts
open Math

let ambient color  = (color, zv, zv)
let diffuse color  = (zv, color, zv)
let specular color = (zv, zv, color)

let phong surface material n e l =
	let (a, d, s) = material in
	let (kd, ks, alpha, _, _) = surface in
	let d = if kd > 0. then d *^ (dotp l n) *^ kd else zv in
	let s = if ks > 0. then
		let r = reflect (zv -^ l) n in
		s *^ ((dotp r e) ** alpha) *^ ks else zv in
	d +^ s

let point_light pos color = Light (ghost, (fun n -> [pos]), color)
