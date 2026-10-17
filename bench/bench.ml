(* Throughput and allocation measurements.

   Two things are worth measuring on a matching engine. Throughput is the
   obvious one. The less obvious one is allocation: in a garbage-collected
   runtime, work done on the hot path is paid for twice, once when you do it
   and again when the collector cleans up after it, and the second payment
   arrives at a time you do not choose.

   The words-per-op column below counts minor-heap words. See [alloc.ml] for
   the attribution: the engine's own structures allocate nothing, and what is
   left is the cost of returning a value. *)

open Base
open Execsim

let p = Stdlib.print_endline
let now () = Unix.gettimeofday ()

let timed name n f =
  let minor_before = Stdlib.Gc.minor_words () in
  let t0 = now () in
  let result = f () in
  let dt = now () -. t0 in
  let words = Stdlib.Gc.minor_words () -. minor_before in
  p
    (Printf.sprintf
       "  %-34s %10.0f ops/sec   %6.0f ns/op   %7.2f words/op"
       name
       (Float.of_int n /. dt)
       (dt *. 1e9 /. Float.of_int n)
       (words /. Float.of_int n));
  result
;;

let floor_px = 0
let ceiling_px = 20_000
let mid = 10_000

let () =
  p "";
  p "execsim benchmarks";
  p "==================";
  p (Printf.sprintf "  OCaml %s" Stdlib.Sys.ocaml_version);
  p "";

  (* ---- resting orders ---- *)
  p "Resting orders (no match)";
  let n = 1_000_000 in
  let b = Book.create ~floor:floor_px ~ceiling:ceiling_px ~capacity:(n + 16) () in
  timed "submit, rests on the book" n (fun () ->
    for i = 1 to n do
      ignore
        (Book.submit_iter
           b
           (Order.limit ~id:i ~side:Buy ~px:(mid - 1 - (i % 500)) ~qty:10 ())
           ~on_fill:(fun ~taker:_ ~maker:_ ~price:_ ~qty:_ -> ())
         : Order.status)
    done);
  p (Printf.sprintf "    %d orders resting across %d levels" (Book.order_count b) 500);

  (* ---- cancels ---- *)
  p "";
  p "Cancellation (the dominant message type on real venues)";
  timed "cancel, O(1) via intrusive unlink" n (fun () ->
    for i = 1 to n do
      match Book.cancel b (Order.Id.of_int i) with
      | `Cancelled _ | `Unknown_id -> ()
    done);
  p (Printf.sprintf "    %d orders left" (Book.order_count b));

  (* ---- matching ---- *)
  p "";
  p "Matching";
  let m = 500_000 in
  let b2 = Book.create ~floor:floor_px ~ceiling:ceiling_px ~capacity:(m + 16) () in
  for i = 1 to m do
    ignore
      (Book.submit_iter
         b2
         (Order.limit ~id:i ~side:Sell ~px:(mid + 1 + (i % 200)) ~qty:10 ())
         ~on_fill:(fun ~taker:_ ~maker:_ ~price:_ ~qty:_ -> ())
       : Order.status)
  done;
  let fills = ref 0 in
  timed "submit_iter, callback per fill" m (fun () ->
    for i = 1 to m do
      ignore
        (Book.submit_iter
           b2
           (Order.market ~id:(10_000_000 + i) ~side:Buy ~qty:10 ())
           ~on_fill:(fun ~taker:_ ~maker:_ ~price:_ ~qty:_ -> Int.incr fills)
         : Order.status)
    done);
  p (Printf.sprintf "    %d fills produced" !fills);

  (* The allocating wrapper, for comparison. *)
  let b3 = Book.create ~floor:floor_px ~ceiling:ceiling_px ~capacity:(m + 16) () in
  for i = 1 to m do
    ignore
      (Book.submit_iter
         b3
         (Order.limit ~id:i ~side:Sell ~px:(mid + 1 + (i % 200)) ~qty:10 ())
         ~on_fill:(fun ~taker:_ ~maker:_ ~price:_ ~qty:_ -> ())
       : Order.status)
  done;
  timed "submit, boxes fills into a list" m (fun () ->
    for i = 1 to m do
      ignore (Book.submit b3 (Order.market ~id:(20_000_000 + i) ~side:Buy ~qty:10 ())
              : Order.outcome)
    done);

  (* ---- sweeps ---- *)
  p "";
  p "Deep sweeps (each order crosses many levels)";
  let b4 = Book.create ~floor:floor_px ~ceiling:ceiling_px ~capacity:300_000 () in
  let levels = 200 in
  let sweeps = 20_000 in
  for i = 1 to levels * sweeps / 20 do
    ignore
      (Book.submit_iter
         b4
         (Order.limit ~id:i ~side:Sell ~px:(mid + 1 + (i % levels)) ~qty:5 ())
         ~on_fill:(fun ~taker:_ ~maker:_ ~price:_ ~qty:_ -> ())
       : Order.status)
  done;
  let swept = ref 0 in
  timed "market order sweeping ~20 levels" sweeps (fun () ->
    for i = 1 to sweeps do
      ignore
        (Book.submit_iter
           b4
           (Order.market ~id:(30_000_000 + i) ~side:Buy ~qty:100 ())
           ~on_fill:(fun ~taker:_ ~maker:_ ~price:_ ~qty:_ -> Int.incr swept)
         : Order.status)
    done);
  p (Printf.sprintf "    %d fills across the sweeps" !swept);

  (* ---- schedules ---- *)
  p "";
  p "Execution schedules";
  let reps = 100_000 in
  let sigma = Array.create ~len:50 28.6 in
  timed "tridiagonal solve, 50 slices" reps (fun () ->
    for _ = 1 to reps do
      ignore
        (Schedule.variable_vol_path
           ~total:10_000.
           ~sigma
           ~risk_aversion:1e-5
           ~eta:0.017
           ~horizon:1.0
         : float array)
    done);
  p "";
  p "Words/op counts minor-heap allocation. Run ./alloc.exe for the breakdown:";
  p "the book's own structures allocate nothing, and the remainder is the cost";
  p "of the returned value -- 2 words for a status, 16 more to box a fill list."
;;
