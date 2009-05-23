(* vector: x, y, z *)
type vec = float * float * float

(* matrix: rows 1, 2, 3 *)
type matrix = vec * vec * vec

(* color: red, green, blue *)
type color = vec

(* ray: origin, direction *)
type ray = vec * vec

(* triangle: vectors 1, 2, 3 *)
type triangle = vec * vec * vec

(* camera: location, facing, up, field-of-view x, y *)
type camera = vec * vec * vec * float * float

(* hit point: distance, normal, point *)
type hit = float * vec * vec

(* intersection function: cull -> ray -> hit option *)
type intersect = bool -> ray -> hit option 

(* bounding volume function: incoming ray -> intersects *)
type bound = ray -> bool

(* surface properties: diffusivity, specularity, shininess, reflectivity, refractivity *)
type surface = float * float * float * float * float

(* physical properties: refraction index, absorption *)
type physics = float * color

(* material properties: ambient, diffuse, specular *)
type material = color * color * color

(* shape: bounding volume, intersection function *)
type shape = bound * intersect

(* object: shape, surface properties, physics *)
type obj = shape * surface * material * physics option

(* sphere: location, radius *)
type sphere = vec * float

(* spherical lens: center, facing normal, radius, half-width *)
type lens = vec * vec * float * float

(* axis-aligned bounding box: minimum and maximum coords *)
type aabb = vec * vec

(* plane: normal, D-value *)
type plane = vec * float 

(* surface sampler: num samples -> point list *)
type sampler = int -> vec list

(* light: shape, surface sampler, color *)
type light = shape * sampler * color

(* entity: either an object or a light *)
type entity = Object of obj | Light of light

(* scene: camera, entities *)
type scene = camera * entity list


exception IllegalAxis
exception FunkySolid


let shape_of entity =
	match entity with
		Object (s,_,_,_) -> s
	  | Light (s,_,_) -> s
