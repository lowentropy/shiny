open Types

let up = (0., 1., 0.)
let ground = up, 0.

let red    = (1.0, 0.0, 0.0)
let green  = (0.0, 1.0, 0.0)
let blue   = (0.0, 0.0, 1.0)
let gray v = (v, v, v)
let white  = gray 1.0
let black  = gray 0.0

let zv = (0., 0., 0.)
let zm = (zv, zv, zv)
let eye = (red, green, blue)

let glass = (1.5, zv)
let air   = (1.0, zv)

let ghost = (fun _ -> false), (fun _ _ -> None)
