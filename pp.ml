open Math
open Format

let print_vec (x,y,z) =
	printf "(%.4f, %.4f, %.4f)" x y z

let print_ray (o, d) =
	print_vec o;
	print_string " -> ";
	print_vec d
