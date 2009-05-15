open Types
open Math
open Draw
open Graphics
open Int


let diffuse color (intersect:ifun) ray =
	match intersect ray with
		None -> None |
		Some (p, n) -> Some (p, n, fun (_:vec) -> color)

let red = diffuse (1.,0.,0.)
let white = diffuse (1.,1.,1.)

let sphere = ((0.,1.,0.),1.)
let vol = sphere_vol sphere
let surf = red (int_sphere sphere)
let phys = (false, 0., 0.)

let obj = (vol, surf, phys)

;;

seed();
draw 500 500 (fun x y -> let s = (x + y) / 4 in (s,s,s));
ignore (wait_next_event [Key_pressed])
