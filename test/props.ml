(* Property-based tests over random operation sequences.

   Unit tests check the cases you thought of. These check the cases you did
   not: QCheck generates thousands of interleavings of submit and cancel, and
   every one of them has to leave the book obeying its stated invariants and
   conserving quantity. When a property fails, QCheck shrinks the sequence to a
   minimal counterexample, which is usually short enough to read directly.

   The properties below are the ones that matter for a matching engine. A book
   that crosses, loses shares, or fills a taker at a price worse than its limit
   is a book that loses money in a way that is extremely hard to find by
   inspection. *)

open Base
open Execsim
module Q = QCheck

let floor_px = 9900
let ceiling_px = 10100

(* ------------------------------------------------------------------ *)
(* generators                                                          *)
(* ------------------------------------------------------------------ *)

type op =
  | Submit of
      { side : Order.Side.t
      ; px : int
      ; qty : int
      ; tif : Order.Tif.t
      }
  | Submit_market of
      { side : Order.Side.t
      ; qty : int
      }
  | Cancel of int (* index into the ids issued so far *)

let gen_side = Q.Gen.(map (fun b -> if b then Order.Side.Buy else Order.Side.Sell) bool)

let gen_tif =
  Q.Gen.frequencyl
    [ 6, Order.Tif.Good_till_cancel
    ; 2, Order.Tif.Immediate_or_cancel
    ; 1, Order.Tif.Fill_or_kill
    ]
;;

let gen_op =
  let open Q.Gen in
  frequency
    [ ( 8
      , gen_side
        >>= fun side ->
        int_range (floor_px - 5) (ceiling_px + 5)
        >>= fun px ->
        int_range 1 500
        >>= fun qty -> gen_tif >>= fun tif -> return (Submit { side; px; qty; tif }) )
    ; ( 1
      , gen_side
        >>= fun side -> int_range 1 500 >>= fun qty -> return (Submit_market { side; qty })
      )
    ; (2, int_range 0 50 >>= fun k -> return (Cancel k))
    ]
;;

let gen_ops = Q.Gen.list_size (Q.Gen.int_range 1 120) gen_op

let print_op = function
  | Submit { side; px; qty; tif } ->
    Printf.sprintf
      "submit %s %d @ %d (%s)"
      (Order.Side.to_string side)
      qty
      px
      (Order.Tif.to_string tif)
  | Submit_market { side; qty } ->
    Printf.sprintf "market %s %d" (Order.Side.to_string side) qty
  | Cancel k -> Printf.sprintf "cancel #%d" k
;;

let arb_ops =
  Q.make gen_ops ~print:(fun ops -> String.concat ~sep:"; " (List.map ops ~f:print_op))
;;

(* ------------------------------------------------------------------ *)
(* a run: replay an operation sequence, accumulating ledger totals      *)
(* ------------------------------------------------------------------ *)

type run =
  { book : Book.t
  ; mutable submitted : int (* taker quantity accepted into the engine *)
  ; mutable traded : int (* quantity that changed hands, counted once *)
  ; mutable killed : int (* IOC/FOK remainders discarded *)
  ; mutable cancelled : int (* quantity removed by explicit cancel *)
  ; mutable ids : int list (* ids issued, newest first *)
  ; mutable failure : string option
  }

let record_failure r msg = if Option.is_none r.failure then r.failure <- Some msg

let check r where =
  match Book.check_invariants r.book with
  | Ok () -> ()
  | Error e -> record_failure r (Printf.sprintf "%s: %s" where e)
;;

let apply r next_id op =
  match op with
  | Cancel k ->
    (match List.nth r.ids k with
     | None -> ()
     | Some id ->
       (match Book.cancel r.book (Order.Id.of_int id) with
        | `Unknown_id -> ()
        | `Cancelled q -> r.cancelled <- r.cancelled + Qty.Pos.to_int q);
       check r "after cancel")
  | Submit_market { side; qty } ->
    let id = next_id () in
    let req = Order.market ~id ~side ~qty () in
    let out = Book.submit r.book req in
    r.ids <- id :: r.ids;
    (match out.status with
     | Order.Rejected _ -> ()
     | Order.Filled | Order.Resting _ | Order.Cancelled_remainder _ ->
       r.submitted <- r.submitted + qty);
    r.traded <- r.traded + Qty.to_int (Order.filled_qty out);
    (match out.status with
     | Order.Cancelled_remainder q -> r.killed <- r.killed + Qty.Pos.to_int q
     | Order.Filled | Order.Resting _ | Order.Rejected _ -> ());
    check r "after market order"
  | Submit { side; px; qty; tif } ->
    let id = next_id () in
    let req = Order.limit ~id ~side ~px ~qty ~tif () in
    let out = Book.submit r.book req in
    r.ids <- id :: r.ids;
    (match out.status with
     | Order.Rejected _ -> ()
     | Order.Filled | Order.Resting _ | Order.Cancelled_remainder _ ->
       r.submitted <- r.submitted + qty);
    r.traded <- r.traded + Qty.to_int (Order.filled_qty out);
    (match out.status with
     | Order.Cancelled_remainder q -> r.killed <- r.killed + Qty.Pos.to_int q
     | Order.Filled | Order.Resting _ | Order.Rejected _ -> ());
    (* A taker must never trade through its own limit. *)
    List.iter out.fills ~f:(fun f ->
      let ok =
        Order.Side.is_aggressive_at
          ~side
          ~limit:(Px.of_ticks px)
          ~resting:f.Order.price
      in
      if not ok
      then
        record_failure
          r
          (Printf.sprintf
             "fill at %s violates %s limit of %d"
             (Px.to_string f.Order.price)
             (Order.Side.to_string side)
             px));
    check r "after limit order"
;;

let run_ops ops =
  let r =
    { book = Book.create ~floor:floor_px ~ceiling:ceiling_px ~capacity:16 ()
    ; submitted = 0
    ; traded = 0
    ; killed = 0
    ; cancelled = 0
    ; ids = []
    ; failure = None
    }
  in
  let counter = ref 0 in
  let next_id () =
    Int.incr counter;
    !counter
  in
  List.iter ops ~f:(fun op -> apply r next_id op);
  r
;;

(* ------------------------------------------------------------------ *)
(* properties                                                          *)
(* ------------------------------------------------------------------ *)

let prop_invariants =
  Q.Test.make
    ~count:2000
    ~name:"book invariants hold after every operation"
    arb_ops
    (fun ops ->
       let r = run_ops ops in
       match r.failure with
       | None -> true
       | Some msg -> Q.Test.fail_report msg)
;;

(* Every share submitted to the engine ends up in exactly one of four places:
   it traded, it is resting, it was killed as an IOC/FOK remainder, or it was
   cancelled. Shares cannot be created or destroyed. *)
let prop_conservation =
  Q.Test.make
    ~count:2000
    ~name:"quantity is conserved"
    arb_ops
    (fun ops ->
       let r = run_ops ops in
       let resting =
         Qty.to_int (Book.total_qty r.book Order.Side.Buy)
         + Qty.to_int (Book.total_qty r.book Order.Side.Sell)
       in
       (* Each trade consumes one taker share and one maker share. Maker shares
          were counted as submitted when they rested, so the traded quantity is
          removed from the ledger twice. *)
       let accounted = (2 * r.traded) + resting + r.killed + r.cancelled in
       if accounted = r.submitted
       then true
       else
         Q.Test.fail_reportf
           "submitted %d but accounted for %d (traded %d x2, resting %d, killed %d, \
            cancelled %d)"
           r.submitted
           accounted
           r.traded
           resting
           r.killed
           r.cancelled)
;;

(* [fillable] inspects the book without mutating it, so it must agree with what
   a marketable order actually gets. Execution strategies size their child
   orders off this, so a wrong answer here is a wrong answer everywhere. *)
let prop_fillable_is_honest =
  Q.Test.make
    ~count:1000
    ~name:"fillable predicts the realised fill exactly"
    (Q.pair arb_ops (Q.make Q.Gen.(pair gen_side (int_range (floor_px - 2) (ceiling_px + 2)))))
    (fun (ops, (side, px)) ->
       let r = run_ops ops in
       match r.failure with
       | Some msg -> Q.Test.fail_reportf "setup broke: %s" msg
       | None ->
         let limit = Px.of_ticks px in
         let predicted = Qty.to_int (Book.fillable r.book ~side ~limit) in
         let big = Int.max 1 (predicted + 10) in
         let out =
           Book.submit
             r.book
             (Order.limit
                ~id:1_000_000
                ~side
                ~px
                ~qty:big
                ~tif:Order.Tif.Immediate_or_cancel
                ())
         in
         let actual = Qty.to_int (Order.filled_qty out) in
         if predicted = actual
         then true
         else Q.Test.fail_reportf "fillable said %d, order filled %d" predicted actual)
;;

(* Fill-or-kill is all or nothing: never a partial fill, never a resting
   remainder. *)
let prop_fok_is_atomic =
  Q.Test.make
    ~count:1000
    ~name:"fill-or-kill fills completely or not at all"
    (Q.pair arb_ops (Q.make Q.Gen.(pair gen_side (int_range 1 900))))
    (fun (ops, (side, qty)) ->
       let r = run_ops ops in
       let px = if Order.Side.equal side Order.Side.Buy then ceiling_px else floor_px in
       let out =
         Book.submit
           r.book
           (Order.limit ~id:2_000_000 ~side ~px ~qty ~tif:Order.Tif.Fill_or_kill ())
       in
       let got = Qty.to_int (Order.filled_qty out) in
       match out.status with
       | Order.Rejected _ ->
         if got = 0 then true else Q.Test.fail_reportf "rejected FOK still filled %d" got
       | Order.Filled ->
         if got = qty
         then true
         else Q.Test.fail_reportf "FOK reported Filled but traded %d of %d" got qty
       | Order.Resting _ -> Q.Test.fail_reportf "FOK rested on the book"
       | Order.Cancelled_remainder _ ->
         Q.Test.fail_reportf "FOK left a remainder instead of being rejected")
;;

(* Cancelling a resting order returns exactly the quantity that was resting,
   and removes exactly that much depth from its price level. *)
let prop_cancel_is_exact =
  Q.Test.make
    ~count:1000
    ~name:"cancel removes exactly the resting quantity"
    (Q.pair arb_ops (Q.make Q.Gen.(int_range 0 60)))
    (fun (ops, k) ->
       let r = run_ops ops in
       match List.nth r.ids k with
       | None -> true
       | Some id ->
         let before_buy = Qty.to_int (Book.total_qty r.book Order.Side.Buy) in
         let before_sell = Qty.to_int (Book.total_qty r.book Order.Side.Sell) in
         (match Book.cancel r.book (Order.Id.of_int id) with
          | `Unknown_id -> true
          | `Cancelled q ->
            let after_buy = Qty.to_int (Book.total_qty r.book Order.Side.Buy) in
            let after_sell = Qty.to_int (Book.total_qty r.book Order.Side.Sell) in
            let removed = before_buy + before_sell - (after_buy + after_sell) in
            if removed = Qty.Pos.to_int q
            then (
              match Book.check_invariants r.book with
              | Ok () -> true
              | Error e -> Q.Test.fail_reportf "invariant after cancel: %s" e)
            else
              Q.Test.fail_reportf
                "cancel reported %d but depth fell by %d"
                (Qty.Pos.to_int q)
                removed))
;;

let tests =
  [ prop_invariants
  ; prop_conservation
  ; prop_fillable_is_honest
  ; prop_fok_is_atomic
  ; prop_cancel_is_exact
  ]
;;
