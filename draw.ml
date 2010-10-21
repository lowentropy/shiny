open Format
open Graphics
open Math
open Pp

let draw normalize w h f =
	open_graph (sprintf " %dx%d" w h);
  if normalize then (
  	set_color (rgb 0 0 0);
  	let colors = Array.init h (fun y ->
  	  Array.init w (fun x ->
  	    plot x (h-y-1);
  	    f w h x y
    )) in
    let m = ref 0.0 in
    if normalize then
      for y = 0 to h-1 do
      for x = 0 to w-1 do
        let r,g,b = Array.get (Array.get colors y) x in
        m := fmax !m (max r (max g b))
  	done done;
  	for y = 0 to h-1 do
  	for x = 0 to w-1 do
  	  let color = Array.get (Array.get colors y) x in
  		let r,g,b = vmap color (fun i ->
  		  let i = if normalize then i /. !m else i in
  		  int_of_float ((fmin i 1.) *. 255.)) in
      set_color (rgb r g b);
  		plot x (h-y-1)
  	done done)
  else (
    for y = 0 to h-1 do
    for x = 0 to w-1 do
      let color = f w h x y in
      let r,g,b = vmap color (fun i ->
        int_of_float ((fmin i 1.) *. 255.)) in
      set_color (rgb r g b);
      plot (h-y-1)
    done done)