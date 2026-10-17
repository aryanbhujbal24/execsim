(** A price–time priority limit order book with an integrated matching engine.

    {1 Representation}

    The book is a dense array of price levels indexed by tick, one array per
    side, over a preallocated pool of orders held in parallel [int array]s. Each
    level owns an intrusive doubly-linked FIFO queue through that pool, so the
    queue needs no cons cells: [next] and [prev] are integer slot indices, and
    [-1] is the null link.

    That representation buys three things:

    - {b O(1) insert.} Append to the tail of the level's queue.
    - {b O(1) cancel.} An id-to-slot index ({!Idmap}) finds the order; the intrusive
      links unlink it without walking the queue. Cancels dominate message
      traffic on real venues, so a design that walks a level to cancel is a
      design that falls over under load.
    - {b A matching path that allocates nothing internally.} The levels, the
      queues and the id index are all preallocated integer arrays, so matching
      an order touches no heap. What allocation remains is the return value and
      nothing else: measured at 2 words for the status variant on
      {!submit_iter}, against 16 for {!submit}, which additionally boxes every
      fill into a list. {!submit_iter} hands fills to a callback with flat
      arguments precisely so that path can be avoided. Numbers come from
      [bench/alloc.ml]; see the README for the full table.

    The cost is a bounded price range, fixed at construction. Real venues apply
    price bands anyway, and a band is exactly what makes the dense array sound.

    {1 Invariants}

    {!check_invariants} states the properties the book is supposed to maintain
    at all times, and is what the property-based tests assert after every
    generated operation sequence. The important one is that the book never
    crosses: after any sequence of submits and cancels, the best bid is strictly
    below the best ask, because anything that would cross has already traded. *)

type t

(** [create ~floor ~ceiling ()] makes an empty book accepting prices in
    [floor, ceiling] ticks inclusive. [capacity] presizes the order pool; the
    pool grows by doubling if exceeded, so the value is a performance hint
    rather than a limit. *)
val create : floor:int -> ceiling:int -> ?capacity:int -> unit -> t

val submit : t -> Order.request -> Order.outcome

(** The non-allocating form of {!submit}. Fills are reported through [on_fill]
    as it goes, with flat arguments so that no fill record is built. *)
val submit_iter
  :  t
  -> Order.request
  -> on_fill:(taker:Order.Id.t -> maker:Order.Id.t -> price:Px.t -> qty:Qty.Pos.t -> unit)
  -> Order.status

val cancel : t -> Order.Id.t -> [ `Cancelled of Qty.Pos.t | `Unknown_id ]
val best_bid : t -> Px.t option
val best_ask : t -> Px.t option
val spread_ticks : t -> int option
val mid_ticks : t -> float option

(** Resting quantity at exactly this price on this side. *)
val depth_at : t -> Order.Side.t -> Px.t -> Qty.t

(** Total resting quantity on a side. *)
val total_qty : t -> Order.Side.t -> Qty.t

(** Live orders resting on both sides. *)
val order_count : t -> int

(** The top [depth] non-empty levels on a side, best first. *)
val levels : t -> Order.Side.t -> depth:int -> (Px.t * Qty.t) list

(** Quantity a taker of [side] could fill up to [limit], without mutating. *)
val fillable : t -> side:Order.Side.t -> limit:Px.t -> Qty.t

(** [Error] carries the first violated invariant, named. *)
val check_invariants : t -> (unit, string) Result.t
