open Base

module Strategy = struct
  type t =
    | Twap
    | Pov of float
    | Almgren_chriss of
        { risk_aversion : float
        ; sigma : float array
        ; eta : float
        }

  let name = function
    | Twap -> "TWAP"
    | Pov f -> Printf.sprintf "POV(%.0f%%)" (f *. 100.)
    | Almgren_chriss _ -> "Almgren-Chriss"
  ;;
end

module Resilience = struct
  type t = { fraction : float }

  let none = { fraction = 0. }
  let partial ~fraction = { fraction = Float.max 0. (Float.min 1. fraction) }
  let full = { fraction = 1. }
end

type result =
  { requested : int
  ; filled : int
  ; arrival_mid_ticks : float
  ; vwap_ticks : float
  ; shortfall_ticks : float
  ; total_cost_ticks : float
  ; slices_traded : int
  ; levels_consumed : int
  }

(* Slice sizes for the schedule-driven strategies. POV cannot be precomputed,
   since it depends on depth at the time each slice is sent. *)
let planned_slices strategy ~total ~slices =
  match strategy with
  | Strategy.Pov _ -> None
  | Strategy.Twap ->
    let path =
      Schedule.sinh_path ~total:(Float.of_int total) ~slices ~kappa:0. ~horizon:1.0
    in
    Some (Schedule.to_trades path)
  | Strategy.Almgren_chriss { risk_aversion; sigma; eta } ->
    let sigma =
      if Array.length sigma >= slices
      then Array.sub sigma ~pos:0 ~len:slices
      else
        Array.init slices ~f:(fun i ->
          if Array.length sigma = 0
          then 1.0
          else sigma.(Int.min i (Array.length sigma - 1)))
    in
    let path =
      Schedule.variable_vol_path
        ~total:(Float.of_int total)
        ~sigma
        ~risk_aversion
        ~eta
        ~horizon:1.0
    in
    Some (Schedule.to_trades path)
;;

(* Convert a float schedule into integer lots that sum to exactly [total].
   Largest-remainder, so rounding never loses or invents a share. *)
let integerise trades ~total =
  let n = Array.length trades in
  let floors = Array.map trades ~f:(fun x -> Int.of_float (Float.round_down x)) in
  let assigned = Array.fold floors ~init:0 ~f:( + ) in
  let short = total - assigned in
  let order =
    Array.init n ~f:(fun i -> i, trades.(i) -. Float.round_down trades.(i))
    |> Array.to_list
    |> List.sort ~compare:(fun (_, a) (_, b) -> Float.compare b a)
  in
  List.iteri order ~f:(fun rank (i, _) -> if rank < short then floors.(i) <- floors.(i) + 1);
  floors
;;

let touch_depth book ~side =
  (* Depth available to a taker of [side] is resting on the opposite side. *)
  match side with
  | Order.Side.Buy ->
    (match Book.best_ask book with
     | None -> 0
     | Some px -> Qty.to_int (Book.depth_at book Order.Side.Sell px))
  | Order.Side.Sell ->
    (match Book.best_bid book with
     | None -> 0
     | Some px -> Qty.to_int (Book.depth_at book Order.Side.Buy px))
;;

let replenish book ~(resilience : Resilience.t) ~side ~consumed ~id =
  if Float.( <= ) resilience.fraction 0. || consumed <= 0
  then id
  else (
    let qty = Int.of_float (Float.round_nearest (Float.of_int consumed *. resilience.fraction)) in
    if qty <= 0
    then id
    else (
      (* Post the refill back at the current touch on the side being consumed. *)
      let maker_side = Order.Side.opposite side in
      let px =
        match maker_side with
        | Order.Side.Sell -> Book.best_ask book
        | Order.Side.Buy -> Book.best_bid book
      in
      match px with
      | None -> id
      | Some px ->
        ignore
          (Book.submit
             book
             (Order.limit
                ~id
                ~side:maker_side
                ~px:(Px.to_ticks px)
                ~qty
                ~tif:Order.Tif.Good_till_cancel
                ())
           : Order.outcome);
        id + 1))
;;

let run ~book ~side ~total ~slices ~strategy ?(resilience = Resilience.none)
  ?(first_id = 900_000) ()
  =
  if slices <= 0 then invalid_arg "Sim.run: slices must be positive";
  if total <= 0 then invalid_arg "Sim.run: total must be positive";
  let arrival =
    match Book.mid_ticks book with
    | Some m -> m
    | None -> Float.of_int 0
  in
  let planned = planned_slices strategy ~total ~slices in
  let plan = Option.map planned ~f:(fun t -> integerise t ~total) in
  let id = ref first_id in
  let next_id () =
    Int.incr id;
    !id
  in
  let remaining = ref total in
  let notional = ref 0 in
  let shares = ref 0 in
  let slices_traded = ref 0 in
  let levels = ref 0 in
  let k = ref 0 in
  while !remaining > 0 && !k < slices do
    let want =
      match plan with
      | Some p -> Int.min p.(!k) !remaining
      | None ->
        (match strategy with
         | Strategy.Pov f ->
           let d = touch_depth book ~side in
           Int.max 1 (Int.min !remaining (Int.of_float (Float.of_int d *. f)))
         | Strategy.Twap | Strategy.Almgren_chriss _ -> !remaining)
    in
    if want <= 0
    then Int.incr k
    else (
      let sweep_px =
        match side with
        | Order.Side.Buy -> Book.best_ask book
        | Order.Side.Sell -> Book.best_bid book
      in
      match sweep_px with
      | None -> k := slices (* no liquidity at all: stop *)
      | Some _ ->
        (* Marketable order: reach across the whole band, take what is there,
           kill the rest. A production child order would be smarter; the point
           here is to measure what the schedule costs, not to add a second
           untested strategy inside the measurement. *)
        let out = Book.submit book (Order.market ~id:(next_id ()) ~side ~qty:want ()) in
        let got = Qty.to_int (Order.filled_qty out) in
        if got > 0
        then (
          Int.incr slices_traded;
          levels
          := !levels
             + (List.map out.fills ~f:(fun f -> Px.to_ticks f.Order.price)
                |> List.dedup_and_sort ~compare:Int.compare
                |> List.length);
          List.iter out.fills ~f:(fun f ->
            let q = Qty.Pos.to_int f.Order.qty in
            notional := !notional + (Px.to_ticks f.Order.price * q);
            shares := !shares + q);
          remaining := !remaining - got;
          id := replenish book ~resilience ~side ~consumed:got ~id:(next_id ()));
        Int.incr k)
  done;
  let filled = !shares in
  let vwap =
    if filled = 0 then arrival else Float.of_int !notional /. Float.of_int filled
  in
  let sign =
    match side with
    | Order.Side.Buy -> 1.
    | Order.Side.Sell -> -1.
  in
  let shortfall = sign *. (vwap -. arrival) in
  { requested = total
  ; filled
  ; arrival_mid_ticks = arrival
  ; vwap_ticks = vwap
  ; shortfall_ticks = shortfall
  ; total_cost_ticks = shortfall *. Float.of_int filled
  ; slices_traded = !slices_traded
  ; levels_consumed = !levels
  }
;;

let to_string r =
  Printf.sprintf
    "filled %d/%d  arrival %.2f  vwap %.4f  shortfall %+.4f ticks/share  cost %+.1f  \
     slices %d  levels %d"
    r.filled
    r.requested
    r.arrival_mid_ticks
    r.vwap_ticks
    r.shortfall_ticks
    r.total_cost_ticks
    r.slices_traded
    r.levels_consumed
;;
