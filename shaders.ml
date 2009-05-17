open Types
open Math

let shade f = fun ((_,n,_):hit) d l -> f n (zv-^d) l

let diffuse color =
	shade (fun n _ l -> color *^ (dotp n l))

let blinn diffuse specular alpha =
	shade (fun n v l ->
		let h = dir (l +^ v) in
		let diff = diffuse *^ (dotp l n) in
		let spec = specular *^ ((dotp n h) ** alpha) in
		diff +^ spec)

let phong diffuse specular alpha =
	shade (fun n v l ->
		let r = reflect (zv -^ l) n in
		let diff = diffuse *^ (dotp l n) in
		let spec = specular *^ ((dotp r v) ** alpha) in
		diff +^ spec)
