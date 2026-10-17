(** Prices, as a count of ticks from an instrument's reference price.

    The representation is deliberately abstract. A price is not an integer you
    may do arbitrary arithmetic on: adding two prices is meaningless, and the
    type system should say so. What you may do is offset a price by a number of
    ticks, and subtract two prices to get a tick distance.

    Exchanges quote in ticks, not in floating-point currency. Keeping prices
    integral all the way through removes an entire class of bugs — a limit at
    101.1 that fails to match a resting order at 101.1 because the two values
    differ in the last bit. *)

type t

val of_ticks : int -> t
val to_ticks : t -> int

(** [add_ticks px n] moves [px] by [n] ticks; [n] may be negative. *)
val add_ticks : t -> int -> t

(** [distance a b] is the signed tick distance [a - b]. Deliberately not
    [( - )], since the result is a different kind of thing from its operands. *)
val distance : t -> t -> int

val compare : t -> t -> int
val equal : t -> t -> bool
val hash : t -> int
val min : t -> t -> t
val max : t -> t -> t
val ( < ) : t -> t -> bool
val ( <= ) : t -> t -> bool
val ( > ) : t -> t -> bool
val ( >= ) : t -> t -> bool
val to_string : t -> string
