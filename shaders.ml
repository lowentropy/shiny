open Types
open Math

let shade f = fun ((_,n,_):hit) d l -> f n (zv-^d) l

let diffuse color =
	shade (fun n _ l -> color *^ (dot n l))

let blinn diffuse specular alpha =
	shade (fun n v l ->
		let h = dir (l +^ v) in
		let diff = diffuse *^ (dot l n) in
		let spec = specular *^ ((dot n h) ** alpha) in
		diff +^ spec)

let phong diffuse specular alpha =
	shade (fun n v l ->
		let r = reflect l n in
		let diff = diffuse *^ (dot l n) in
		let spec = specular *^ ((dot r v) ** alpha) in
		diff +^ spec)
