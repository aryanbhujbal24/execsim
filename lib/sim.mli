(** Running an execution schedule against the book.

    A schedule computed from a model is a prediction. This runs it against the
    actual matching engine — walking real price levels, paying the real spread,
    exhausting real depth — and reports what it cost. The gap between the two is
    the point: a model that says slicing finely is optimal will say so whether
    or not the book can absorb the slices, and only simulation shows the
    difference.

    Cost is reported as implementation shortfall against the arrival mid: the
    price when the parent order was born, before any of it traded. That is the
    benchmark a client's money is actually measured against, and unlike VWAP it
    cannot be gamed by trading when the benchmark is favourable. *)

module Strategy : sig
  type t =
    | Twap  (** equal slices; the straight-line holdings path *)
    | Pov of float
        (** participate at this fraction of visible depth at the touch *)
    | Almgren_chriss of
        { risk_aversion : float
        ; sigma : float array  (** volatility forecast, one entry per slice *)
        ; eta : float  (** temporary impact coefficient *)
        }

  val name : t -> string
end

(** How the book recovers between slices. Real books refill after being hit;
    a simulation without resilience makes every strategy look like it is
    trading into a vacuum, and flatters patient schedules enormously. *)
module Resilience : sig
  type t

  (** No replenishment: depth taken is depth gone. *)
  val none : t

  (** After each slice, restore up to [fraction] of the depth consumed, posted
      back at the levels it came from. *)
  val partial : fraction:float -> t

  val full : t
end

type result =
  { requested : int
  ; filled : int
  ; arrival_mid_ticks : float
  ; vwap_ticks : float
  ; shortfall_ticks : float
      (** signed cost per share against arrival mid; positive is worse *)
  ; total_cost_ticks : float  (** shortfall times shares filled *)
  ; slices_traded : int
  ; levels_consumed : int  (** total price levels crossed, a crude impact proxy *)
  }

val run
  :  book:Book.t
  -> side:Order.Side.t
  -> total:int
  -> slices:int
  -> strategy:Strategy.t
  -> ?resilience:Resilience.t
  -> ?first_id:int
  -> unit
  -> result

val to_string : result -> string
