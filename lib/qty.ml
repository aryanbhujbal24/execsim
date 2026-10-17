open Base

type t = int

let zero = 0
let of_int n = if n < 0 then None else Some n
let of_int_exn n = if n < 0 then failwith "Qty.of_int_exn: negative" else n
let to_int t = t
let ( + ) = Int.( + )
let sub a b = if b > a then None else Some (a - b)
let min = Int.min
let is_zero t = t = 0
let compare = Int.compare
let equal = Int.equal
let ( < ) = Int.( < )
let ( <= ) = Int.( <= )
let ( > ) = Int.( > )
let ( >= ) = Int.( >= )
let to_string = Int.to_string

module Pos = struct
  type nonrec t = t

  let of_qty q = if q <= 0 then None else Some q
  let of_int n = if n <= 0 then None else Some n
  let of_int_exn n = if n <= 0 then failwith "Qty.Pos.of_int_exn: not positive" else n
  let to_qty t = t
  let to_int t = t
  let sub p q = if q >= p then `Exhausted else `Remaining (p - q)
  let min_qty p q = Int.min p q
  let compare = Int.compare
  let equal = Int.equal
  let to_string = Int.to_string
end
