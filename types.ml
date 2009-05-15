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

(* camera: position, field-of-view x, y *)
type cam = vec * float * float

(* reflectance function: light direction -> multiplier *)
type refl = vec -> vec

(* hit point: location, normal, reflection *)
type hit = vec * vec * refl

(* intersection function: ray -> point, normal *)
type ifun = ray -> (vec * vec) option 

(* physical properties: refraction index, has-thickness, absorption *)
type phys = float * bool * vec

(* bounding volume function: incoming ray -> intersects *)
type bvol = ray -> bool

(* surface properties: incoming ray -> hit point *)
type surf = ray -> hit option

(* object: bounding volume, surface and interior properties *)
type obj = bvol * surf * phys

(* sphere: location, radius *)
type sphere = vec * float

(* axis-aligned bounding box: minimum and maximum coords *)
type aabb = vec * vec

(* plane: normal, D-value *)
type plane = vec * float 


exception IllegalAxis

let zv = (0., 0., 0.)
let zm = (zv, zv, zv)

let up = (0., 1., 0.)
let ground = up, 0.
