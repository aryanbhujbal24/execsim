(** Optimal execution trajectories.

    A trader liquidating a position faces a trade-off with two sides. Trading
    quickly pushes the price against you; trading slowly leaves you exposed to
    the price moving on its own. Almgren–Chriss makes that precise by
    minimising

    {v  Σ η̃ nₖ² / τ  +  λ Σ σₖ² τ xₖ²  v}

    over the holdings path [x₀ = X, …, x_N = 0], where [nₖ] is the quantity
    traded in slice [k].

    {1 Two solvers}

    With constant volatility the stationarity condition is a linear second-order
    difference equation whose solution is the familiar

    {v  xₖ / X = sinh(κ̃ (T − tₖ)) / sinh(κ̃ T)  v}

    — {!sinh_path}. Note the tilde. The decay rate appearing in the discrete
    solution is {e not} the continuous [κ = sqrt(λσ²/η̃)] that most
    presentations quote; it is the [κ̃] satisfying
    [2(cosh(κ̃τ) − 1)/τ² = κ²]. The two agree only as [τ → 0], and
    {!kappa_tilde} computes the one the discrete problem actually has.

    If volatility varies over the day — and intraday volatility is forecastable,
    so assuming it constant discards information — the closed form no longer
    applies. But the objective stays quadratic in [x] with a tridiagonal
    Hessian, so the optimum still solves a linear system, in O(N) by the Thomas
    algorithm: {!variable_vol_path}. The sinh solution is the special case of
    that system with a flat volatility path, which {!val-sinh_path} and
    {!variable_vol_path} agreeing to numerical precision is a good way to check
    both. *)

(** The decay rate of the discrete problem, distinct from [sqrt (lambda *. sigma
    ** 2. /. eta)]. *)
val kappa_tilde : kappa:float -> tau:float -> float

(** [sinh_path ~total ~slices ~kappa ~horizon] is the holdings remaining after
    each slice, from [total] down to [0], under constant volatility. The result
    has [slices + 1] entries. [kappa] of zero gives the straight line, which is
    a TWAP. *)
val sinh_path : total:float -> slices:int -> kappa:float -> horizon:float -> float array

(** The general solver. [sigma] gives the volatility forecast for each of the
    [slices] intervals. With a flat [sigma] this reproduces {!sinh_path}. *)
val variable_vol_path
  :  total:float
  -> sigma:float array
  -> risk_aversion:float
  -> eta:float
  -> horizon:float
  -> float array

(** Quantities to trade in each slice: the negated first difference of a
    holdings path. Always [slices] entries, summing to the original total. *)
val to_trades : float array -> float array

(** Expected cost of a holdings path, in the same units as the objective:
    impact cost plus risk penalty. Useful for checking that a solver actually
    found the minimum. *)
val objective
  :  path:float array
  -> sigma:float array
  -> risk_aversion:float
  -> eta:float
  -> horizon:float
  -> float
