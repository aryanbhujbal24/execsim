(* Unit tests for the cases worth naming.

   The property tests cover the space; these pin the specific behaviours a
   reader would want to confirm by eye, and the ones where getting it wrong is
   both easy and expensive — price improvement, time priority, and the
   difference between an order that rests and one that is killed. *)

open Base
open Execsim

let book () = Book.create ~floor:9900 ~ceiling:10100 ~capacity:8 ()
let check_px = Alcotest.(check (option int))
let check_int = Alcotest.(check int)

let submit b req = Book.submit b req

let status_name : Order.status -> string = function
  | Order.Filled -> "filled"
  | Order.Resting _ -> "resting"
  | Order.Cancelled_remainder _ -> "cancelled_remainder"
  | Order.Rejected _ -> "rejected"
;;

let check_status msg expected (out : Order.outcome) =
  Alcotest.(check string) msg expected (status_name out.status)
;;

let ticks = Option.map ~f:Px.to_ticks

let assert_sound b =
  match Book.check_invariants b with
  | Ok () -> ()
  | Error e -> Alcotest.failf "invariant violated: %s" e
;;

(* ------------------------------------------------------------------ *)

let test_empty () =
  let b = book () in
  check_px "no bid" None (ticks (Book.best_bid b));
  check_px "no ask" None (ticks (Book.best_ask b));
  check_int "no orders" 0 (Book.order_count b);
  Alcotest.(check (option int)) "no spread" None (Book.spread_ticks b);
  assert_sound b
;;

let test_rest_and_top () =
  let b = book () in
  ignore (submit b (Order.limit ~id:1 ~side:Buy ~px:9990 ~qty:10 ()) : Order.outcome);
  ignore (submit b (Order.limit ~id:2 ~side:Buy ~px:9995 ~qty:20 ()) : Order.outcome);
  ignore (submit b (Order.limit ~id:3 ~side:Sell ~px:10005 ~qty:30 ()) : Order.outcome);
  check_px "best bid is the highest" (Some 9995) (ticks (Book.best_bid b));
  check_px "best ask is the lowest" (Some 10005) (ticks (Book.best_ask b));
  check_int "spread" 10 (Option.value_exn (Book.spread_ticks b));
  check_int "depth at 9995" 20 (Qty.to_int (Book.depth_at b Buy (Px.of_ticks 9995)));
  check_int "depth where nothing rests" 0
    (Qty.to_int (Book.depth_at b Buy (Px.of_ticks 9991)));
  check_int "three live orders" 3 (Book.order_count b);
  assert_sound b
;;

(* The resting order sets the price, so an aggressive buyer willing to pay
   10010 against an offer at 10005 trades at 10005 and keeps the difference.
   Getting this backwards is a silent, systematic transfer of money. *)
let test_price_improvement () =
  let b = book () in
  ignore (submit b (Order.limit ~id:1 ~side:Sell ~px:10005 ~qty:50 ()) : Order.outcome);
  let out = submit b (Order.limit ~id:2 ~side:Buy ~px:10010 ~qty:50 ()) in
  check_status "fully filled" "filled" out;
  check_int "one fill" 1 (List.length out.fills);
  let f = List.hd_exn out.fills in
  check_int "traded at the maker's price" 10005 (Px.to_ticks f.price);
  assert_sound b
;;

(* Two orders at the same price: the one that arrived first fills first. *)
let test_time_priority () =
  let b = book () in
  ignore (submit b (Order.limit ~id:1 ~side:Sell ~px:10000 ~qty:40 ()) : Order.outcome);
  ignore (submit b (Order.limit ~id:2 ~side:Sell ~px:10000 ~qty:40 ()) : Order.outcome);
  let out = submit b (Order.limit ~id:3 ~side:Buy ~px:10000 ~qty:60 ()) in
  check_int "two fills" 2 (List.length out.fills);
  let first = List.hd_exn out.fills in
  let second = List.nth_exn out.fills 1 in
  check_int "oldest maker filled first" 1 (Order.Id.to_int first.maker);
  check_int "and filled in full" 40 (Qty.Pos.to_int first.qty);
  check_int "newer maker filled second" 2 (Order.Id.to_int second.maker);
  check_int "for the remainder" 20 (Qty.Pos.to_int second.qty);
  check_int "20 still resting" 20 (Qty.to_int (Book.total_qty b Sell));
  assert_sound b
;;

let test_sweeps_levels () =
  let b = book () in
  ignore (submit b (Order.limit ~id:1 ~side:Sell ~px:10000 ~qty:10 ()) : Order.outcome);
  ignore (submit b (Order.limit ~id:2 ~side:Sell ~px:10001 ~qty:10 ()) : Order.outcome);
  ignore (submit b (Order.limit ~id:3 ~side:Sell ~px:10002 ~qty:10 ()) : Order.outcome);
  let out = submit b (Order.limit ~id:4 ~side:Buy ~px:10001 ~qty:25 ()) in
  check_int "crossed two levels" 2 (List.length out.fills);
  check_int "filled 20" 20 (Qty.to_int (Order.filled_qty out));
  check_status "rest of the order rests" "resting" out;
  check_px "and becomes the new best bid" (Some 10001) (ticks (Book.best_bid b));
  check_px "leaving the untouched level as the ask" (Some 10002) (ticks (Book.best_ask b));
  assert_sound b
;;

let test_ioc_kills_remainder () =
  let b = book () in
  ignore (submit b (Order.limit ~id:1 ~side:Sell ~px:10000 ~qty:10 ()) : Order.outcome);
  let out =
    submit
      b
      (Order.limit ~id:2 ~side:Buy ~px:10000 ~qty:50 ~tif:Order.Tif.Immediate_or_cancel ())
  in
  check_status "remainder killed" "cancelled_remainder" out;
  check_int "took what was there" 10 (Qty.to_int (Order.filled_qty out));
  check_px "nothing left resting on the bid" None (ticks (Book.best_bid b));
  assert_sound b
;;

let test_fok_all_or_nothing () =
  let b = book () in
  ignore (submit b (Order.limit ~id:1 ~side:Sell ~px:10000 ~qty:10 ()) : Order.outcome);
  let short =
    submit
      b
      (Order.limit ~id:2 ~side:Buy ~px:10000 ~qty:50 ~tif:Order.Tif.Fill_or_kill ())
  in
  check_status "cannot fill, so rejected" "rejected" short;
  check_int "and nothing traded" 0 (Qty.to_int (Order.filled_qty short));
  check_int "resting liquidity untouched" 10 (Qty.to_int (Book.total_qty b Sell));
  let exact =
    submit
      b
      (Order.limit ~id:3 ~side:Buy ~px:10000 ~qty:10 ~tif:Order.Tif.Fill_or_kill ())
  in
  check_status "exact size fills" "filled" exact;
  assert_sound b
;;

let test_market_order () =
  let b = book () in
  let dry = submit b (Order.market ~id:1 ~side:Buy ~qty:10 ()) in
  check_status "no liquidity, so rejected" "rejected" dry;
  ignore (submit b (Order.limit ~id:2 ~side:Sell ~px:10050 ~qty:5 ()) : Order.outcome);
  ignore (submit b (Order.limit ~id:3 ~side:Sell ~px:10060 ~qty:5 ()) : Order.outcome);
  let out = submit b (Order.market ~id:4 ~side:Buy ~qty:8 ()) in
  check_int "reaches across levels" 8 (Qty.to_int (Order.filled_qty out));
  check_status "fully filled" "filled" out;
  assert_sound b
;;

let test_rejections () =
  let b = book () in
  ignore (submit b (Order.limit ~id:1 ~side:Buy ~px:10000 ~qty:10 ()) : Order.outcome);
  let dup = submit b (Order.limit ~id:1 ~side:Buy ~px:10000 ~qty:10 ()) in
  check_status "duplicate id" "rejected" dup;
  let oob = submit b (Order.limit ~id:2 ~side:Buy ~px:50 ~qty:10 ()) in
  check_status "price out of band" "rejected" oob;
  check_int "neither reached the book" 1 (Book.order_count b);
  assert_sound b
;;

let test_cancel () =
  let b = book () in
  ignore (submit b (Order.limit ~id:1 ~side:Buy ~px:10000 ~qty:10 ()) : Order.outcome);
  ignore (submit b (Order.limit ~id:2 ~side:Buy ~px:10000 ~qty:20 ()) : Order.outcome);
  (match Book.cancel b (Order.Id.of_int 1) with
   | `Unknown_id -> Alcotest.fail "live order reported unknown"
   | `Cancelled q -> check_int "cancelled its full size" 10 (Qty.Pos.to_int q));
  check_int "depth drops by exactly that" 20
    (Qty.to_int (Book.depth_at b Buy (Px.of_ticks 10000)));
  (match Book.cancel b (Order.Id.of_int 1) with
   | `Unknown_id -> ()
   | `Cancelled _ -> Alcotest.fail "cancelled the same order twice");
  (* Cancelling the last order at a level must retreat the top of book. *)
  (match Book.cancel b (Order.Id.of_int 2) with
   | `Unknown_id -> Alcotest.fail "live order reported unknown"
   | `Cancelled _ -> ());
  check_px "bid side is now empty" None (ticks (Book.best_bid b));
  assert_sound b
;;

(* The pool starts at 8 slots; resting more than that must grow it without
   corrupting the free list or the intrusive queues. *)
let test_pool_growth () =
  let b = book () in
  for i = 1 to 200 do
    ignore (submit b (Order.limit ~id:i ~side:Buy ~px:(9900 + (i % 50)) ~qty:3 ())
            : Order.outcome)
  done;
  check_int "all resting" 200 (Book.order_count b);
  check_int "total depth" 600 (Qty.to_int (Book.total_qty b Buy));
  assert_sound b;
  for i = 1 to 200 do
    match Book.cancel b (Order.Id.of_int i) with
    | `Cancelled _ -> ()
    | `Unknown_id -> Alcotest.failf "order %d went missing" i
  done;
  check_int "book is empty again" 0 (Book.order_count b);
  check_int "and holds no depth" 0 (Qty.to_int (Book.total_qty b Buy));
  assert_sound b
;;

let test_levels_view () =
  let b = book () in
  List.iter [ 9990, 10; 9995, 20; 9998, 30 ] ~f:(fun (px, qty) ->
    ignore (submit b (Order.limit ~id:px ~side:Buy ~px ~qty ()) : Order.outcome));
  let got = Book.levels b Buy ~depth:2 |> List.map ~f:(fun (p, q) -> Px.to_ticks p, Qty.to_int q) in
  Alcotest.(check (list (pair int int))) "best two levels, best first"
    [ 9998, 30; 9995, 20 ] got
;;

let test_fillable_matches_reality () =
  let b = book () in
  ignore (submit b (Order.limit ~id:1 ~side:Sell ~px:10000 ~qty:10 ()) : Order.outcome);
  ignore (submit b (Order.limit ~id:2 ~side:Sell ~px:10005 ~qty:15 ()) : Order.outcome);
  check_int "within one level" 10
    (Qty.to_int (Book.fillable b ~side:Buy ~limit:(Px.of_ticks 10000)));
  check_int "across both" 25
    (Qty.to_int (Book.fillable b ~side:Buy ~limit:(Px.of_ticks 10005)));
  check_int "below the market" 0
    (Qty.to_int (Book.fillable b ~side:Buy ~limit:(Px.of_ticks 9999)));
  assert_sound b
;;

let tests =
  [ "empty book", `Quick, test_empty
  ; "resting orders and top of book", `Quick, test_rest_and_top
  ; "taker gets the maker's price", `Quick, test_price_improvement
  ; "price-time priority", `Quick, test_time_priority
  ; "sweeping several levels", `Quick, test_sweeps_levels
  ; "immediate-or-cancel", `Quick, test_ioc_kills_remainder
  ; "fill-or-kill", `Quick, test_fok_all_or_nothing
  ; "market orders", `Quick, test_market_order
  ; "rejections", `Quick, test_rejections
  ; "cancellation", `Quick, test_cancel
  ; "order pool growth", `Quick, test_pool_growth
  ; "depth view", `Quick, test_levels_view
  ; "fillable agrees with execution", `Quick, test_fillable_matches_reality
  ]
;;
