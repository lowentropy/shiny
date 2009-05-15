open Format
open Graphics
open Math

let draw w h f =
	open_graph (sprintf " %dx%d" w h);
	for y = 0 to h-1 do
	for x = 0 to w-1 do
		let r, g, b = f x y in
		set_color (rgb r g b);
		plot x (h-y-1)
	done done
