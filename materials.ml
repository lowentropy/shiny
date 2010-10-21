open Math
open Consts
open Shaders

let shiny diffuse specular = phong (diffuse, 0.5) (specular, 0.5) 1000.
let dull diffuse = lambert (diffuse, 0.5)

let reflective reflect =        ((black, 0.), (black, 0.), reflect)
let emissive energy reflect =   (energy,      (black, 0.), reflect)
let absorptive energy reflect = ((black, 0.), energy,      reflect)

let shiny_red = reflective (shiny red white)
let shiny_green = reflective (shiny green white)
let shiny_blue = reflective (shiny blue white)
let shiny_white = reflective (shiny white white)

let dull_white = absorptive (white, 0.5) (dull white)
let dull_red = emissive (red, 1.0) (dull black)
let dull_green = absorptive (white, 0.5) (dull green)

let mirror = reflective (phong (black, 0.0) (white, 1.0) 1000.0)
let transparent = reflective (dull black)

let blue_glass = ((white -^ blue, 0.5), 1.5)
let red_glass = ((white -^ red, 0.5), 1.5)
let green_glass = ((white -^ green, 0.5), 1.5)
let clear_glass = ((black, 0.0), 1.5)