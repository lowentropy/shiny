type vec = float * float * float
type mat = vec * vec * vec
type color = vec

exception IllegalAxis

(*
	components of types

		hierarchical transform structure
		objects decompose to patches for radiosity
		calculation of bounding boxes
		sharing of point data
*)

type material = color
type surf = material
type obj = surf list (** bbfun*)

let zv = (0., 0., 0.)
let zm = (zv, zv, zv)
