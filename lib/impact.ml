open Base

type t =
  { lambda : float
  ; delta : float
  ; permanent_fraction : float
  }

let create ~lambda ~delta ~permanent_fraction =
  if Float.( < ) lambda 0. then invalid_arg "Impact.create: lambda must be non-negative";
  if Float.( <= ) delta 0. then invalid_arg "Impact.create: delta must be positive";
  if Float.( < ) permanent_fraction 0. || Float.( > ) permanent_fraction 1.
  then invalid_arg "Impact.create: permanent fraction must lie in [0, 1]";
  { lambda; delta; permanent_fraction }
;;

let linear ~lambda = create ~lambda ~delta:1.0 ~permanent_fraction:0.5
let square_root ~lambda = create ~lambda ~delta:0.5 ~permanent_fraction:0.5

(* Estimated from 2.02M CME E-mini Nasdaq-100 trades matched to the prevailing
   quote. The exponent's 95% confidence interval was [0.605, 0.769], which
   excludes both 1.0 and 0.5. *)
let estimated = create ~lambda:0.243 ~delta:0.687 ~permanent_fraction:0.93

let lambda t = t.lambda
let delta t = t.delta
let permanent_fraction t = t.permanent_fraction

let price_move t ~signed_qty ~depth =
  if Float.( <= ) depth 0. || Float.( = ) signed_qty 0.
  then 0.
  else (
    let sign = if Float.( < ) signed_qty 0. then -1. else 1. in
    let participation = Float.abs signed_qty /. depth in
    sign *. t.lambda *. (participation **. t.delta))
;;

let permanent_move t ~signed_qty ~depth =
  price_move t ~signed_qty ~depth *. t.permanent_fraction
;;

let temporary_move t ~signed_qty ~depth =
  price_move t ~signed_qty ~depth *. (1. -. t.permanent_fraction)
;;

let cost_per_share t ~signed_qty ~depth =
  if Float.( = ) signed_qty 0.
  then 0.
  else Float.abs (price_move t ~signed_qty ~depth)
;;
