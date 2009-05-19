(* vector: x, y, z *)
type vec = float * float * float

(* matrix: rows 1, 2, 3 *)
type mat = vec * vec * vec

(* color: red, green, blue *)
type color = vec

(* ray: origin, direction *)
type ray = vec * vec

(* triangle: vectors 1, 2, 3 *)
type tri = vec * vec * vec

(* camera: location, facing, up, field-of-view x, y *)
type cam = vec * vec * vec * float * float

(* hit point: distance, normal, point *)
type hit = float * vec * vec

(* reflectance function: hit point -> eye ray -> light direction -> multiplier *)
type refl = hit -> vec -> vec -> vec

(* intersection function: ray -> hit option *)
type ifun = ray -> hit option 

(* bounding volume function: incoming ray -> intersects *)
type bvol = ray -> bool

(* surface properties: diffusivity, specularity, reflectivity, refractivity *)
type surf = float * float * float * float

(* physical properties: refraction index, has-thickness, absorption *)
type phys = float * bool * vec

(* material properties: ambient, diffuse, specular *)
type material = color * color * color

(* shape: bounding volume, intersection function *)
type shape = bvol * ifun

(* object: shape, surface properties, physics *)
type obj = shape * surf * phys

(* sphere: location, radius *)
type sphere = vec * float

(* axis-aligned bounding box: minimum and maximum coords *)
type aabb = vec * vec

(* plane: normal, D-value *)
type plane = vec * float 

(* light function: num samples -> index -> point *)
type lfun = int -> int -> vec

(* light: point selector, shape, color *)
type light = lfun * shape * color

(* scene: objects, lights, camera *)
type scene = obj list * light list * cam


exception IllegalAxis

let zv = (0., 0., 0.)
let zm = (zv, zv, zv)

let up = (0., 1., 0.)
let ground = up, 0.

let red =   (1.0, 0.0, 0.0)
let green = (0.0, 1.0, 0.0)
let blue =  (0.0, 0.0, 1.0)
let gray v = (v, v, v)
let white = gray 1.0
