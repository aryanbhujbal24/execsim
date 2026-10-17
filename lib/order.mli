(** Orders, fills, and the result of submitting an order.

    Every type here is a closed sum. There is no [Other] constructor and no
    integer status code, so adding a new time-in-force or a new rejection reason
    turns every site that must handle it into a compile error. That is the whole
    argument for writing a matching engine in a language with exhaustive
    matching: the compiler, rather than production, tells you where the new case
    was not considered. *)

module Side : sig
  type t =
    | Buy
    | Sell

  val opposite : t -> t
  val equal : t -> t -> bool
  val to_string : t -> string

  (** [is_aggressive_at ~side ~limit ~resting] is whether an order of [side]
      with limit price [limit] is willing to trade against [resting]. A buy
      crosses when its limit is at or above the offer; a sell when its limit is
      at or below the bid. *)
  val is_aggressive_at : side:t -> limit:Px.t -> resting:Px.t -> bool
end

module Id : sig
  type t

  val of_int : int -> t
  val to_int : t -> int
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val hash : t -> int
  val to_string : t -> string
end

module Tif : sig
  type t =
    | Good_till_cancel  (** rest on the book until filled or cancelled *)
    | Immediate_or_cancel  (** take what is available, kill the remainder *)
    | Fill_or_kill  (** fill the whole quantity at once, or nothing at all *)

  val to_string : t -> string
end

module Kind : sig
  type t =
    | Limit of Px.t
    | Market
end

type request =
  { id : Id.t
  ; side : Side.t
  ; kind : Kind.t
  ; qty : Qty.Pos.t
  ; tif : Tif.t
  }

val limit : id:int -> side:Side.t -> px:int -> qty:int -> ?tif:Tif.t -> unit -> request
val market : id:int -> side:Side.t -> qty:int -> unit -> request

type fill =
  { taker : Id.t
  ; maker : Id.t
  ; price : Px.t  (** always the resting order's price: the maker sets the price *)
  ; qty : Qty.Pos.t
  }

module Reject : sig
  type t =
    | Duplicate_id
    | Fill_or_kill_unfillable
    | Market_order_no_liquidity
    | Price_out_of_range

  val to_string : t -> string
end

(** What happened to a submitted order. The quantity carried by [Resting] and
    [Cancelled_remainder] is positive by construction, so neither can describe
    an order of size zero. *)
type status =
  | Filled
  | Resting of Qty.Pos.t
  | Cancelled_remainder of Qty.Pos.t
  | Rejected of Reject.t

type outcome =
  { fills : fill list  (** in execution order *)
  ; status : status
  }

val filled_qty : outcome -> Qty.t

(** Volume-weighted average fill price in ticks, or [None] if nothing traded. *)
val vwap_ticks : outcome -> float option
