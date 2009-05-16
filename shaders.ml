open Types
open Math

let diffuse color (intersect:ifun) ray =
	match intersect ray with
		None -> None |
		Some (p, n) -> Some (p, n, fun (_:vec) -> color)
