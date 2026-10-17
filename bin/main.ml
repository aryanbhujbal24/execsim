(* Demonstration driver.

   Builds a synthetic book, runs three execution strategies against it under
   identical conditions, and reports what each cost. The schedules come from
   [Schedule]; the costs come from actually trading them through the matching
   engine, not from the model that produced them. *)

open Base
open Execsim

let p = Stdlib.print_endline
let section title = p ""; p title; p (String.make (String.length title) '-')

let floor_px = 9000
let ceiling_px = 11000
let mid = 10000

(* A book with geometrically thinning depth away from the touch, which is
   roughly what a liquid future looks like. Deterministic, so the comparison
   between strategies is like for like. *)
let build_book ~levels ~touch_qty =
  let b = Book.create ~floor:floor_px ~ceiling:ceiling_px ~capacity:4096 () in
  let id = ref 0 in
  for i = 0 to levels - 1 do
    let qty = Int.max 1 (Int.of_float (Float.of_int touch_qty *. (1.15 **. Float.of_int i))) in
    Int.incr id;
    ignore
      (Book.submit b (Order.limit ~id:!id ~side:Sell ~px:(mid + 1 + i) ~qty ())
       : Order.outcome);
    Int.incr id;
    ignore
      (Book.submit b (Order.limit ~id:!id ~side:Buy ~px:(mid - 1 - i) ~qty ())
       : Order.outcome)
  done;
  b
;;

let () =
  section "Book";
  let b = build_book ~levels:40 ~touch_qty:120 in
  p (Printf.sprintf "  %d resting orders" (Book.order_count b));
  List.iter
    [ Order.Side.Sell, "asks"; Order.Side.Buy, "bids" ]
    ~f:(fun (side, label) ->
      let view =
        Book.levels b side ~depth:5
        |> List.map ~f:(fun (px, q) ->
          Printf.sprintf "%s x%s" (Px.to_string px) (Qty.to_string q))
        |> String.concat ~sep:"  "
      in
      p (Printf.sprintf "  %-5s %s" label view));
  p
    (Printf.sprintf
       "  spread %d ticks, mid %.1f"
       (Option.value_exn (Book.spread_ticks b))
       (Option.value_exn (Book.mid_ticks b)));

  section "Schedules";
  let slices = 20 in
  let total = 6000 in
  let horizon = 1.0 in
  let eta = 0.017 in
  let lam = 2e-6 in
  let sig0 = 28.6 in
  let kappa = Float.sqrt (lam *. sig0 *. sig0 /. eta) in
  let tau = horizon /. Float.of_int slices in
  p (Printf.sprintf "  kappa      %.5f   (continuous)" kappa);
  p
    (Printf.sprintf
       "  kappa~     %.5f   (discrete; the one the difference equation has)"
       (Schedule.kappa_tilde ~kappa ~tau));
  let flat = Array.create ~len:slices sig0 in
  let ac = Schedule.sinh_path ~total:(Float.of_int total) ~slices ~kappa ~horizon in
  let tri =
    Schedule.variable_vol_path
      ~total:(Float.of_int total)
      ~sigma:flat
      ~risk_aversion:lam
      ~eta
      ~horizon
  in
  let gap =
    Array.foldi ac ~init:0. ~f:(fun i acc x -> Float.max acc (Float.abs (x -. tri.(i))))
  in
  p (Printf.sprintf "  closed form vs tridiagonal solver: max gap %.2e shares" gap);
  (* A U-shaped intraday volatility path: busy open, quiet midday, busy close. *)
  let u_shaped =
    Array.init slices ~f:(fun i ->
      let u = Float.of_int i /. Float.of_int (slices - 1) in
      sig0 *. (0.7 +. (1.8 *. ((u -. 0.5) **. 2.))))
  in
  let tri_u =
    Schedule.variable_vol_path
      ~total:(Float.of_int total)
      ~sigma:u_shaped
      ~risk_aversion:lam
      ~eta
      ~horizon
  in
  let pct arr k = 100. *. (1. -. (arr.(k) /. Float.of_int total)) in
  p "";
  p "  percent completed by slice:";
  p "    slice      5      10      15      20";
  p
    (Printf.sprintf
       "    TWAP   %5.1f   %5.1f   %5.1f   %5.1f"
       (100. *. 5. /. 20.)
       (100. *. 10. /. 20.)
       (100. *. 15. /. 20.)
       100.);
  p
    (Printf.sprintf
       "    AC     %5.1f   %5.1f   %5.1f   %5.1f"
       (pct ac 5)
       (pct ac 10)
       (pct ac 15)
       (pct ac 20));
  p
    (Printf.sprintf
       "    AC-U   %5.1f   %5.1f   %5.1f   %5.1f   (U-shaped volatility)"
       (pct tri_u 5)
       (pct tri_u 10)
       (pct tri_u 15)
       (pct tri_u 20));

  section "Impact models";
  p "  trading 600 lots into 1200 of visible depth:";
  List.iter
    [ "linear (Almgren-Chriss)", Impact.linear ~lambda:0.243
    ; "square-root law", Impact.square_root ~lambda:0.243
    ; "estimated from CME NQ", Impact.estimated
    ]
    ~f:(fun (name, m) ->
      p
        (Printf.sprintf
           "    %-24s delta %.3f   move %.4f   permanent %.4f"
           name
           (Impact.delta m)
           (Impact.price_move m ~signed_qty:600. ~depth:1200.)
           (Impact.permanent_move m ~signed_qty:600. ~depth:1200.)));

  section "Execution, simulated through the matching engine";
  p "  buying 6000 lots, 20 slices, book replenishing 60% between slices";
  let strategies =
    [ Sim.Strategy.Twap
    ; Sim.Strategy.Pov 0.5
    ; Sim.Strategy.Almgren_chriss { risk_aversion = lam; sigma = flat; eta }
    ; Sim.Strategy.Almgren_chriss { risk_aversion = lam; sigma = u_shaped; eta }
    ]
  in
  List.iteri strategies ~f:(fun i strategy ->
    let b = build_book ~levels:40 ~touch_qty:120 in
    let r =
      Sim.run
        ~book:b
        ~side:Order.Side.Buy
        ~total
        ~slices
        ~strategy
        ~resilience:(Sim.Resilience.partial ~fraction:0.6)
        ~first_id:(100_000 * (i + 2))
        ()
    in
    let label =
      if i = 3 then "AC (U-shaped vol)" else Sim.Strategy.name strategy
    in
    p (Printf.sprintf "    %-20s %s" label (Sim.to_string r)));
  p "";
  p "  Shortfall is per share against the arrival mid; positive is worse."
;;
