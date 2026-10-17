(* Tests for the execution-schedule solvers.

   The important one is {!test_solvers_agree}: the closed-form sinh trajectory
   and the general tridiagonal solver are independent derivations of the same
   optimum, so under constant volatility they must coincide. If they do not,
   one of them is wrong, and the test says so without needing a reference
   implementation to compare against. This is the same discipline as checking
   an estimator against simulated data whose answer you already know. *)

open Base
open Execsim

let close ?(tol = 1e-8) msg a b =
  if Float.( > ) (Float.abs (a -. b)) tol
  then Alcotest.failf "%s: %.12f vs %.12f (tolerance %g)" msg a b tol
;;

let eta = 0.017
let sig0 = 28.6
let horizon = 1.0

let test_twap_is_linear () =
  let path = Schedule.sinh_path ~total:1000. ~slices:10 ~kappa:0. ~horizon in
  Array.iteri path ~f:(fun k x ->
    close (Printf.sprintf "slice %d" k) x (1000. *. (1. -. (Float.of_int k /. 10.))))
;;

let test_boundaries () =
  let path = Schedule.sinh_path ~total:500. ~slices:17 ~kappa:0.9 ~horizon in
  close "starts holding everything" path.(0) 500.;
  close "ends holding nothing" path.(Array.length path - 1) 0.;
  (* Liquidation is monotone: you never buy back while selling down. *)
  Array.iteri path ~f:(fun k x ->
    if k > 0 && Float.( > ) x path.(k - 1)
    then Alcotest.failf "holdings rose at slice %d" k)
;;

(* The discrete decay rate is not the continuous one, and conflating them is a
   real and silent error. It should satisfy 2(cosh(k~ tau) - 1)/tau^2 = kappa^2
   exactly, and approach kappa as tau shrinks. *)
let test_kappa_tilde () =
  let kappa = 0.7 in
  List.iter [ 0.5; 0.1; 0.01 ] ~f:(fun tau ->
    let kt = Schedule.kappa_tilde ~kappa ~tau in
    let lhs = 2. *. (Float.cosh (kt *. tau) -. 1.) /. (tau *. tau) in
    close ~tol:1e-9 (Printf.sprintf "defining relation at tau=%g" tau) lhs (kappa *. kappa));
  let coarse = Schedule.kappa_tilde ~kappa ~tau:0.5 in
  let fine = Schedule.kappa_tilde ~kappa ~tau:0.001 in
  if Float.( >= ) (Float.abs (fine -. kappa)) (Float.abs (coarse -. kappa))
  then Alcotest.fail "kappa~ should approach kappa as tau shrinks";
  close ~tol:1e-6 "and converge to it" fine kappa
;;

let test_solvers_agree () =
  let total = 10_000. in
  List.iter [ 5; 20; 50; 200 ] ~f:(fun slices ->
    List.iter [ 1e-7; 1e-5; 1e-3 ] ~f:(fun lam ->
      let kappa = Float.sqrt (lam *. sig0 *. sig0 /. eta) in
      let closed = Schedule.sinh_path ~total ~slices ~kappa ~horizon in
      let tri =
        Schedule.variable_vol_path
          ~total
          ~sigma:(Array.create ~len:slices sig0)
          ~risk_aversion:lam
          ~eta
          ~horizon
      in
      Array.iteri closed ~f:(fun k x ->
        close
          ~tol:1e-6
          (Printf.sprintf "slices=%d lambda=%g slice %d" slices lam k)
          x
          tri.(k))))
;;

(* The solver is supposed to find a minimum, so perturbing the path in any
   direction must make the objective worse. *)
let test_optimum_is_optimal () =
  let total = 10_000. in
  let slices = 24 in
  let lam = 1e-5 in
  let sigma = Array.create ~len:slices sig0 in
  let best =
    Schedule.variable_vol_path ~total ~sigma ~risk_aversion:lam ~eta ~horizon
  in
  let base = Schedule.objective ~path:best ~sigma ~risk_aversion:lam ~eta ~horizon in
  List.iter [ 1; 7; 13; 23 ] ~f:(fun k ->
    List.iter [ 1.0; -1.0; 25.0; -25.0 ] ~f:(fun bump ->
      let perturbed = Array.copy best in
      perturbed.(k) <- perturbed.(k) +. bump;
      let worse =
        Schedule.objective ~path:perturbed ~sigma ~risk_aversion:lam ~eta ~horizon
      in
      if Float.( < ) worse (base -. 1e-9)
      then
        Alcotest.failf
          "perturbing slice %d by %+.1f improved the objective: %.9f < %.9f"
          k
          bump
          worse
          base))
;;

(* Higher risk aversion means front-loading: get the position down faster and
   accept more impact to do it. *)
let test_risk_aversion_front_loads () =
  let total = 1000. in
  let slices = 20 in
  let half_done path =
    Array.findi path ~f:(fun _ x -> Float.( <= ) x (total /. 2.))
    |> Option.value_map ~default:slices ~f:fst
  in
  let patient = Schedule.sinh_path ~total ~slices ~kappa:0.05 ~horizon in
  let urgent = Schedule.sinh_path ~total ~slices ~kappa:4.0 ~horizon in
  if half_done urgent >= half_done patient
  then
    Alcotest.failf
      "urgent schedule reached the halfway point at slice %d, patient at %d"
      (half_done urgent)
      (half_done patient)
;;

let test_trades_sum_to_total () =
  let total = 7777. in
  let path = Schedule.sinh_path ~total ~slices:31 ~kappa:1.3 ~horizon in
  let trades = Schedule.to_trades path in
  Alcotest.(check int) "one trade per slice" 31 (Array.length trades);
  close ~tol:1e-9 "trades sum to the parent order" (Array.fold trades ~init:0. ~f:( +. )) total;
  Array.iteri trades ~f:(fun k q ->
    if Float.( < ) q 0. then Alcotest.failf "slice %d trades a negative quantity" k)
;;

(* A U-shaped volatility day should pull trading away from the volatile open
   and close, relative to a flat forecast. *)
let test_variable_vol_responds_to_forecast () =
  let total = 1000. in
  let slices = 40 in
  let lam = 1e-4 in
  let flat = Array.create ~len:slices sig0 in
  let spiky =
    Array.init slices ~f:(fun i -> if i < slices / 2 then sig0 *. 3. else sig0 *. 0.3)
  in
  let a = Schedule.variable_vol_path ~total ~sigma:flat ~risk_aversion:lam ~eta ~horizon in
  let b = Schedule.variable_vol_path ~total ~sigma:spiky ~risk_aversion:lam ~eta ~horizon in
  (* With volatility concentrated early, holding inventory through the first
     half is expensive, so more should be sold before the midpoint. *)
  let mid = slices / 2 in
  if Float.( >= ) b.(mid) a.(mid)
  then
    Alcotest.failf
      "front-loaded volatility did not front-load trading: %.2f remaining vs %.2f"
      b.(mid)
      a.(mid)
;;

let test_impact_models () =
  let m = Impact.estimated in
  Alcotest.(check (float 1e-9)) "exponent" 0.687 (Impact.delta m);
  Alcotest.(check (float 1e-9)) "permanent share" 0.93 (Impact.permanent_fraction m);
  close "no trade, no move" (Impact.price_move m ~signed_qty:0. ~depth:100.) 0.;
  (* Concavity: doubling the size less than doubles the move. *)
  let one = Impact.price_move m ~signed_qty:100. ~depth:1000. in
  let two = Impact.price_move m ~signed_qty:200. ~depth:1000. in
  if Float.( >= ) two (2. *. one)
  then Alcotest.failf "impact is not concave: %.6f vs %.6f" two (2. *. one);
  (* And selling moves the price the other way, symmetrically. *)
  close
    "symmetric in sign"
    (Impact.price_move m ~signed_qty:(-100.) ~depth:1000.)
    (-.one);
  (* Permanent plus temporary is the whole move. *)
  close
    "decomposition is complete"
    (Impact.permanent_move m ~signed_qty:100. ~depth:1000.
     +. Impact.temporary_move m ~signed_qty:100. ~depth:1000.)
    one
;;

let tests =
  [ "TWAP is the straight line", `Quick, test_twap_is_linear
  ; "schedule boundaries and monotonicity", `Quick, test_boundaries
  ; "discrete decay rate", `Quick, test_kappa_tilde
  ; "closed form agrees with tridiagonal solver", `Quick, test_solvers_agree
  ; "the optimum really is optimal", `Quick, test_optimum_is_optimal
  ; "risk aversion front-loads", `Quick, test_risk_aversion_front_loads
  ; "trades reconstruct the parent order", `Quick, test_trades_sum_to_total
  ; "schedule responds to the volatility forecast", `Quick, test_variable_vol_responds_to_forecast
  ; "impact model properties", `Quick, test_impact_models
  ]
;;
