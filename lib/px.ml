open Base

type t = int

let of_ticks t = t
let to_ticks t = t
let add_ticks t n = t + n
let distance a b = a - b
let compare = Int.compare
let equal = Int.equal
let hash = Int.hash
let min = Int.min
let max = Int.max
let ( < ) = Int.( < )
let ( <= ) = Int.( <= )
let ( > ) = Int.( > )
let ( >= ) = Int.( >= )
let to_string = Int.to_string
