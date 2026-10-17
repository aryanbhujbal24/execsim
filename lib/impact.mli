(** Market impact models.

    The textbook execution literature assumes impact is linear in trading rate,
    because linearity is what makes the optimal schedule solvable in closed
    form. Measured impact is not linear. Fitting

    {v  Δp = λ · sign(q) · |q / V| ^ δ  v}

    to trades matched against the prevailing quote gives an exponent well below
    one: {!estimated} carries δ = 0.687, which rejects both the linear
    assumption (δ = 1) and the square-root law (δ = 0.5) that is usually
    substituted for it.

    The second parameter that matters is how much of the move is permanent.
    Impact that permanently repositions the mid is a cost the trader pays
    whatever schedule they choose, so it drops out of the optimisation
    entirely; only the temporary part is worth scheduling around. At 93%
    permanent there is far less to optimise than the linear model suggests,
    which is a result about the problem rather than about the solver. *)

type t

val create : lambda:float -> delta:float -> permanent_fraction:float -> t

(** Linear impact, as assumed by Almgren–Chriss. *)
val linear : lambda:float -> t

(** The square-root law, δ = 1/2. *)
val square_root : lambda:float -> t

(** Impact as estimated from CME E-mini Nasdaq-100 trades: δ = 0.687, 93%
    permanent. Replace the constants here when you re-estimate. *)
val estimated : t

val lambda : t -> float
val delta : t -> float
val permanent_fraction : t -> float

(** Total price move, in ticks, from trading [signed_qty] against a book
    showing [depth] at the touch. Positive quantity buys and pushes the price
    up. *)
val price_move : t -> signed_qty:float -> depth:float -> float

(** The part of {!price_move} that persists after the trade. *)
val permanent_move : t -> signed_qty:float -> depth:float -> float

(** The part that decays, and which scheduling can avoid. *)
val temporary_move : t -> signed_qty:float -> depth:float -> float

(** Impact per share traded. Concave impact means this {e falls} as size rises,
    which is why trading in large slices is less bad than a linear model
    predicts, and why the linear model over-recommends slicing. *)
val cost_per_share : t -> signed_qty:float -> depth:float -> float
