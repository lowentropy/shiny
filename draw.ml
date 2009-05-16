open Format
open Graphics
open Math

let draw w h f =
	open_graph (sprintf " %dx%d" w h);
	for y = 0 to h-1 do
	for x = 0 to w-1 do
		let color = f w h x y in
		let r,g,b = vmap color (fun i ->
			int_of_float (i *. 255.0)) in
		set_color (rgb r g b);
		plot x (h-y-1)
	done done
