(** Share quantities.

    Two invariants are enforced by the type system rather than by discipline:

    - A [Qty.t] is never negative. There is no constructor that can produce one,
      and [sub] returns an option rather than wrapping around.

    - A [Qty.Pos.t] is never zero. This is the type of quantity that a resting
      order has. An order with nothing left to fill is not an order resting at
      zero — it has left the book. Encoding that in the type means the book
      cannot hold a phantom order with no size, because such a value cannot be
      constructed.

    The payoff shows up in {!Pos.sub}: taking a fill out of a resting quantity
    returns a variant the caller must match on, so the case where the order is
    exhausted and must be unlinked cannot be silently forgotten. *)

type t

val zero : t
val of_int : int -> t option
val of_int_exn : int -> t
val to_int : t -> int
val ( + ) : t -> t -> t

(** [sub a b] is [None] when [b > a], rather than a negative quantity. *)
val sub : t -> t -> t option

val min : t -> t -> t
val is_zero : t -> bool
val compare : t -> t -> int
val equal : t -> t -> bool
val ( < ) : t -> t -> bool
val ( <= ) : t -> t -> bool
val ( > ) : t -> t -> bool
val ( >= ) : t -> t -> bool
val to_string : t -> string

module Pos : sig
  type qty := t

  (** A strictly positive quantity. *)
  type t

  val of_qty : qty -> t option
  val of_int : int -> t option
  val of_int_exn : int -> t
  val to_qty : t -> qty
  val to_int : t -> int

  (** [sub p q] removes [q] from [p]. [`Exhausted] means [q >= p]: the order is
      fully consumed and must be removed from the book. *)
  val sub : t -> qty -> [ `Remaining of t | `Exhausted ]

  val min_qty : t -> qty -> qty
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val to_string : t -> string
end
