open Types
open Builders
open Math

let sphere_uv p =
  let x,y,z = dir p in
  let phi = acos y in
  let theta = acos (z /. (sin phi)) in
  let u = phi in
  let v = if x > 0. then theta else 1. -. theta in
  (u, v)

let rec subdivide_sphere (o,r) (a,b,c) levels =
  let (a,b,c) = vmap (a,b,c) dir in
  let t = vmap (a,b,c) (fun x -> x *^ r +^ o) in
  let uv = vmap (a,b,c) sphere_uv in
  if levels == 0
    then (t, uv, []) else
  let edge_point a b =
    dir ((a +^ b) /^ 2.) in
  let levels = levels - 1 in
  let u = edge_point a b in
  let v = edge_point b c in
  let w = edge_point c a in
  ( t, uv,
    [ subdivide_sphere (o,r) (u,v,w) levels;
      subdivide_sphere (o,r) (a,u,w) levels;
      subdivide_sphere (o,r) (b,v,u) levels;
      subdivide_sphere (o,r) (c,w,v) levels;
  ])

let make_sphere_tree (c,r) n = List.map (fun t -> subdivide_sphere (c,r) t n) icosahedron_tris

let rec flatten_subdivision_tree lst all =
  List.fold_left (fun p (t,_,sub) ->
    match sub with [] -> t::p | _ -> (flatten_subdivision_tree sub p)
  ) all lst

let subdivided_sphere_tris (c,r) n = flatten_subdivision_tree (make_sphere_tree (c,r) n) []