open Base

module Side = struct
  type t =
    | Buy
    | Sell

  let opposite = function
    | Buy -> Sell
    | Sell -> Buy
  ;;

  let equal a b =
    match a, b with
    | Buy, Buy | Sell, Sell -> true
    | Buy, Sell | Sell, Buy -> false
  ;;

  let to_string = function
    | Buy -> "buy"
    | Sell -> "sell"
  ;;

  let is_aggressive_at ~side ~limit ~resting =
    match side with
    | Buy -> Px.( >= ) limit resting
    | Sell -> Px.( <= ) limit resting
  ;;
end

module Id = struct
  type t = int

  let of_int t = t
  let to_int t = t
  let equal = Int.equal
  let compare = Int.compare
  let hash = Int.hash
  let to_string = Int.to_string
end

module Tif = struct
  type t =
    | Good_till_cancel
    | Immediate_or_cancel
    | Fill_or_kill

  let to_string = function
    | Good_till_cancel -> "GTC"
    | Immediate_or_cancel -> "IOC"
    | Fill_or_kill -> "FOK"
  ;;
end

module Kind = struct
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

let limit ~id ~side ~px ~qty ?(tif = Tif.Good_till_cancel) () =
  { id = Id.of_int id
  ; side
  ; kind = Kind.Limit (Px.of_ticks px)
  ; qty = Qty.Pos.of_int_exn qty
  ; tif
  }
;;

let market ~id ~side ~qty () =
  { id = Id.of_int id
  ; side
  ; kind = Kind.Market
  ; qty = Qty.Pos.of_int_exn qty
  ; tif = Tif.Immediate_or_cancel
  }
;;

type fill =
  { taker : Id.t
  ; maker : Id.t
  ; price : Px.t
  ; qty : Qty.Pos.t
  }

module Reject = struct
  type t =
    | Duplicate_id
    | Fill_or_kill_unfillable
    | Market_order_no_liquidity
    | Price_out_of_range

  let to_string = function
    | Duplicate_id -> "duplicate order id"
    | Fill_or_kill_unfillable -> "fill-or-kill could not be filled in full"
    | Market_order_no_liquidity -> "market order found no liquidity"
    | Price_out_of_range -> "price outside the book's tick range"
  ;;
end

type status =
  | Filled
  | Resting of Qty.Pos.t
  | Cancelled_remainder of Qty.Pos.t
  | Rejected of Reject.t

type outcome =
  { fills : fill list
  ; status : status
  }

let filled_qty t =
  List.fold t.fills ~init:Qty.zero ~f:(fun acc f -> Qty.( + ) acc (Qty.Pos.to_qty f.qty))
;;

let vwap_ticks t =
  match t.fills with
  | [] -> None
  | fills ->
    let notional, shares =
      List.fold fills ~init:(0, 0) ~f:(fun (n, s) f ->
        let q = Qty.Pos.to_int f.qty in
        n + (Px.to_ticks f.price * q), s + q)
    in
    if shares = 0 then None else Some (Float.of_int notional /. Float.of_int shares)
;;
