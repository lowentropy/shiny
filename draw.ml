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
      plot x (h-y-1)
    done done)

let output_le c i =
  output_byte c ((i lsr 0)  land 255);
  output_byte c ((i lsr 8)  land 255);
  output_byte c ((i lsr 16) land 255);
  output_byte c ((i lsr 24) land 255)
        
let save_bmp colors w h i =
  let filename = sprintf "output/frame%04d.bmp" i in
  let c = open_out_bin filename in
  let bpp = 24 in
  let rowsize = 4 * int_of_float (ceil ((float (bpp * w)) /. 32.)) in
  let padding = rowsize - (bpp * w / 8) in
  let blobsize = rowsize * h in
  let filesize = blobsize + 54 in
  output_string c "BM";
  output_le c filesize;
  output_le c 0;
  output_le c 54;
  output_le c 40;
  output_le c w;
  output_le c h;
  output_byte c 1;
  output_byte c 0;
  output_byte c bpp;
  output_byte c 0;
  output_le c 0;
  output_le c blobsize;
  output_le c 2835;
  output_le c 2835;
  output_le c 0;
  output_le c 0;
  Array.iter (fun row ->
    Array.iter (fun (r,g,b) ->
      output_byte c b;
      output_byte c g;
      output_byte c r;
    ) row;
    for i = 1 to padding do
      output_byte c 0;
    done
  ) colors;
  close_out c

let draw_movie w h f u s =
  let rec dm s i =
    match s with None -> () | Some s_ ->
    let colors = Array.init h (fun y ->
      Array.init w (fun x ->
        let color = f s_ w h x (h - y - 1) in
        vmap color (fun i -> int_of_float ((fmin i 1.) *. 255.))
    )) in
    save_bmp colors w h i;
    dm (u s_) (i+1) in
  dm (Some s) 1
