(** An allocation-free map from order id to pool slot.

    The book needs to find an order by id on every cancel, and cancels are the
    most common message on a real venue. [Base.Hashtbl] is the obvious choice
    and the wrong one here: it boxes keys and values, returns [int option] from
    lookups, and allocates on insertion and removal — measured at seven words
    per set-and-remove pair, which on this path is the entire allocation
    budget.

    This is open addressing with linear probing over two preallocated
    [int array]s. Keys and values are unboxed, lookups return a sentinel rather
    than an option, and nothing is allocated except when the table grows. The
    cost is that it only maps [int] to [int] and reserves two key values, which
    is exactly the generality the book needs and no more. *)

type t

(** [not_found] is returned by {!find} for an absent key, instead of [None]. *)
val not_found : int

(** [create n] sizes the table for about [n] entries. *)
val create : int -> t

(** [find t key] is the stored value, or {!not_found}. *)
val find : t -> int -> int

val mem : t -> int -> bool
val set : t -> key:int -> data:int -> unit

(** Removes [key] if present. Removing an absent key is a no-op. *)
val remove : t -> int -> unit

val length : t -> int
val clear : t -> unit
