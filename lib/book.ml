open Base

let null = -1

type t =
  { floor : int (* tick value of level index 0 *)
  ; n_levels : int
  ; (* --- order pool: structure of arrays, indexed by slot --- *)
    mutable o_id : int array
  ; mutable o_qty : int array (* remaining; > 0 while live *)
  ; mutable o_level : int array (* level index *)
  ; mutable o_is_buy : bool array
  ; mutable o_next : int array
  ; mutable o_prev : int array
  ; mutable o_seq : int array (* arrival order, for priority assertions *)
  ; mutable capacity : int
  ; mutable free_head : int (* head of the free-slot list, via o_next *)
  ; (* --- price levels, one pair of queues per side --- *)
    bid_head : int array
  ; bid_tail : int array
  ; bid_qty : int array
  ; ask_head : int array
  ; ask_tail : int array
  ; ask_qty : int array
  ; (* --- cached book top; -1 / n_levels mean "side is empty" --- *)
    mutable best_bid : int
  ; mutable best_ask : int
  ; index : Idmap.t (* order id -> pool slot, allocation-free *)
  ; mutable live : int
  ; mutable seq : int
  }

let create ~floor ~ceiling ?(capacity = 4096) () =
  if ceiling < floor then invalid_arg "Book.create: ceiling below floor";
  if capacity <= 0 then invalid_arg "Book.create: capacity must be positive";
  let n_levels = ceiling - floor + 1 in
  let t =
    { floor
    ; n_levels
    ; o_id = Array.create ~len:capacity 0
    ; o_qty = Array.create ~len:capacity 0
    ; o_level = Array.create ~len:capacity 0
    ; o_is_buy = Array.create ~len:capacity false
    ; o_next = Array.create ~len:capacity null
    ; o_prev = Array.create ~len:capacity null
    ; o_seq = Array.create ~len:capacity 0
    ; capacity
    ; free_head = 0
    ; bid_head = Array.create ~len:n_levels null
    ; bid_tail = Array.create ~len:n_levels null
    ; bid_qty = Array.create ~len:n_levels 0
    ; ask_head = Array.create ~len:n_levels null
    ; ask_tail = Array.create ~len:n_levels null
    ; ask_qty = Array.create ~len:n_levels 0
    ; best_bid = null
    ; best_ask = n_levels
    ; index = Idmap.create capacity
    ; live = 0
    ; seq = 0
    }
  in
  (* Thread the free list through o_next. *)
  for i = 0 to capacity - 1 do
    t.o_next.(i) <- (if i = capacity - 1 then null else i + 1)
  done;
  t
;;

(* ------------------------------------------------------------------ *)
(* pool                                                                *)
(* ------------------------------------------------------------------ *)

let grow t =
  let old_cap = t.capacity in
  let new_cap = old_cap * 2 in
  let extend arr default =
    let a = Array.create ~len:new_cap default in
    Array.blit ~src:arr ~src_pos:0 ~dst:a ~dst_pos:0 ~len:old_cap;
    a
  in
  t.o_id <- extend t.o_id 0;
  t.o_qty <- extend t.o_qty 0;
  t.o_level <- extend t.o_level 0;
  t.o_is_buy <- extend t.o_is_buy false;
  t.o_prev <- extend t.o_prev null;
  t.o_seq <- extend t.o_seq 0;
  let next = extend t.o_next null in
  for i = old_cap to new_cap - 1 do
    next.(i) <- (if i = new_cap - 1 then null else i + 1)
  done;
  t.o_next <- next;
  t.capacity <- new_cap;
  t.free_head <- old_cap
;;

let alloc_slot t =
  if t.free_head = null then grow t;
  let slot = t.free_head in
  t.free_head <- t.o_next.(slot);
  slot
;;

let free_slot t slot =
  t.o_next.(slot) <- t.free_head;
  t.o_prev.(slot) <- null;
  t.o_qty.(slot) <- 0;
  t.free_head <- slot
;;

(* ------------------------------------------------------------------ *)
(* level queues                                                        *)
(* ------------------------------------------------------------------ *)

let heads t ~is_buy = if is_buy then t.bid_head else t.ask_head
let tails t ~is_buy = if is_buy then t.bid_tail else t.ask_tail
let qtys t ~is_buy = if is_buy then t.bid_qty else t.ask_qty

let rec scan_down arr i = if i < 0 then null else if arr.(i) > 0 then i else scan_down arr (i - 1)

let rec scan_up arr n i =
  if i >= n then n else if arr.(i) > 0 then i else scan_up arr n (i + 1)
;;

let push_tail t ~slot ~is_buy ~level =
  let head = heads t ~is_buy
  and tail = tails t ~is_buy
  and qty = qtys t ~is_buy in
  let prev = tail.(level) in
  t.o_prev.(slot) <- prev;
  t.o_next.(slot) <- null;
  if prev = null then head.(level) <- slot else t.o_next.(prev) <- slot;
  tail.(level) <- slot;
  qty.(level) <- qty.(level) + t.o_qty.(slot);
  if is_buy
  then (if level > t.best_bid then t.best_bid <- level)
  else if level < t.best_ask
  then t.best_ask <- level
;;

(* Unlink [slot] from its level queue. Does not touch the level's cached
   quantity: callers adjust that themselves, since a partial fill changes the
   quantity without unlinking. *)
let unlink t ~slot =
  let is_buy = t.o_is_buy.(slot) in
  let level = t.o_level.(slot) in
  let head = heads t ~is_buy
  and tail = tails t ~is_buy in
  let prev = t.o_prev.(slot)
  and next = t.o_next.(slot) in
  if prev = null then head.(level) <- next else t.o_next.(prev) <- next;
  if next = null then tail.(level) <- prev else t.o_prev.(next) <- prev
;;

let repair_top t ~is_buy ~level =
  let qty = qtys t ~is_buy in
  if qty.(level) = 0
  then
    if is_buy
    then (if level = t.best_bid then t.best_bid <- scan_down qty (level - 1))
    else if level = t.best_ask
    then t.best_ask <- scan_up qty t.n_levels (level + 1)
;;

(* ------------------------------------------------------------------ *)
(* prices                                                              *)
(* ------------------------------------------------------------------ *)

let level_of_px t px = Px.to_ticks px - t.floor
let px_of_level t level = Px.of_ticks (t.floor + level)
let in_range t level = level >= 0 && level < t.n_levels

let best_bid t = if t.best_bid = null then None else Some (px_of_level t t.best_bid)

let best_ask t =
  if t.best_ask >= t.n_levels then None else Some (px_of_level t t.best_ask)
;;

let spread_ticks t =
  if t.best_bid = null || t.best_ask >= t.n_levels
  then None
  else Some (t.best_ask - t.best_bid)
;;

let mid_ticks t =
  if t.best_bid = null || t.best_ask >= t.n_levels
  then None
  else
    Some
      (Float.of_int (Px.to_ticks (px_of_level t t.best_bid)
                     + Px.to_ticks (px_of_level t t.best_ask))
       /. 2.)
;;

let depth_at t side px =
  let level = level_of_px t px in
  if not (in_range t level)
  then Qty.zero
  else (
    let is_buy = Order.Side.equal side Order.Side.Buy in
    Qty.of_int_exn (qtys t ~is_buy).(level))
;;

let total_qty t side =
  let is_buy = Order.Side.equal side Order.Side.Buy in
  Qty.of_int_exn (Array.fold (qtys t ~is_buy) ~init:0 ~f:( + ))
;;

let order_count t = t.live

let levels t side ~depth =
  let is_buy = Order.Side.equal side Order.Side.Buy in
  let qty = qtys t ~is_buy in
  let rec go i taken acc =
    if taken >= depth || not (in_range t i)
    then List.rev acc
    else if qty.(i) > 0
    then
      go
        (if is_buy then i - 1 else i + 1)
        (taken + 1)
        ((px_of_level t i, Qty.of_int_exn qty.(i)) :: acc)
    else go (if is_buy then i - 1 else i + 1) taken acc
  in
  let start = if is_buy then t.best_bid else t.best_ask in
  if (is_buy && start = null) || ((not is_buy) && start >= t.n_levels)
  then []
  else go start 0 []
;;

(* ------------------------------------------------------------------ *)
(* matching                                                            *)
(* ------------------------------------------------------------------ *)

(* The highest (buy) or lowest (sell) level index a taker is willing to reach.
   A market order reaches the whole book. *)
let out_of_band = Int.min_value

let aggression_bound t ~side ~(kind : Order.Kind.t) =
  match kind, side with
  | Order.Kind.Market, Order.Side.Buy -> t.n_levels - 1
  | Order.Kind.Market, Order.Side.Sell -> 0
  | Order.Kind.Limit px, _ ->
    let level = level_of_px t px in
    if in_range t level then level else out_of_band
;;

let fillable_from t ~side ~bound =
  let is_buy_taker = Order.Side.equal side Order.Side.Buy in
  (* A buy taker consumes asks upward; a sell taker consumes bids downward. *)
  let qty = qtys t ~is_buy:(not is_buy_taker) in
  let rec go i acc =
    if is_buy_taker
    then if i > bound || i >= t.n_levels then acc else go (i + 1) (acc + qty.(i))
    else if i < bound || i < 0
    then acc
    else go (i - 1) (acc + qty.(i))
  in
  go (if is_buy_taker then t.best_ask else t.best_bid) 0
;;

let fillable t ~side ~limit =
  let bound = aggression_bound t ~side ~kind:(Order.Kind.Limit limit) in
  if bound = out_of_band then Qty.zero else Qty.of_int_exn (fillable_from t ~side ~bound)
;;

(* Consume resting liquidity, reporting each fill. Returns what is left of the
   taker's quantity. Mutates the book. *)
let consume t ~taker_id ~side ~bound ~qty ~on_fill =
  let is_buy_taker = Order.Side.equal side Order.Side.Buy in
  let maker_is_buy = not is_buy_taker in
  let heads_ = heads t ~is_buy:maker_is_buy in
  let lvl_qty = qtys t ~is_buy:maker_is_buy in
  let remaining = ref qty in
  let continue_ = ref true in
  while !continue_ && !remaining > 0 do
    let level = if is_buy_taker then t.best_ask else t.best_bid in
    let exhausted_side =
      if is_buy_taker then level >= t.n_levels else level = null
    in
    let past_bound = if is_buy_taker then level > bound else level < bound in
    if exhausted_side || past_bound
    then continue_ := false
    else (
      (* Walk this level's FIFO queue, oldest first: time priority. *)
      let price = px_of_level t level in
      while !remaining > 0 && heads_.(level) <> null do
        let maker = heads_.(level) in
        let avail = t.o_qty.(maker) in
        let traded = Int.min avail !remaining in
        t.o_qty.(maker) <- avail - traded;
        lvl_qty.(level) <- lvl_qty.(level) - traded;
        remaining := !remaining - traded;
        on_fill
          ~taker:taker_id
          ~maker:(Order.Id.of_int t.o_id.(maker))
          ~price
          ~qty:(Qty.Pos.of_int_exn traded);
        if t.o_qty.(maker) = 0
        then (
          unlink t ~slot:maker;
          Idmap.remove t.index t.o_id.(maker);
          free_slot t maker;
          t.live <- t.live - 1)
      done;
      repair_top t ~is_buy:maker_is_buy ~level)
  done;
  !remaining
;;

let rest t ~(req : Order.request) ~qty ~level =
  let slot = alloc_slot t in
  let is_buy = Order.Side.equal req.side Order.Side.Buy in
  t.o_id.(slot) <- Order.Id.to_int req.id;
  t.o_qty.(slot) <- qty;
  t.o_level.(slot) <- level;
  t.o_is_buy.(slot) <- is_buy;
  t.seq <- t.seq + 1;
  t.o_seq.(slot) <- t.seq;
  push_tail t ~slot ~is_buy ~level;
  Idmap.set t.index ~key:(Order.Id.to_int req.id) ~data:slot;
  t.live <- t.live + 1
;;

let submit_iter t (req : Order.request) ~on_fill =
  if Idmap.mem t.index (Order.Id.to_int req.id)
  then Order.Rejected Order.Reject.Duplicate_id
  else (
    let bound = aggression_bound t ~side:req.side ~kind:req.kind in
    if bound = out_of_band
    then Order.Rejected Order.Reject.Price_out_of_range
    else (
      let wanted = Qty.Pos.to_int req.qty in
      let unfillable_fok =
        match req.tif with
        | Order.Tif.Fill_or_kill -> fillable_from t ~side:req.side ~bound < wanted
        | Order.Tif.Good_till_cancel | Order.Tif.Immediate_or_cancel -> false
      in
      if unfillable_fok
      then Order.Rejected Order.Reject.Fill_or_kill_unfillable
      else (
        let left = consume t ~taker_id:req.id ~side:req.side ~bound ~qty:wanted ~on_fill in
        if left = 0
        then Order.Filled
        else (
          match req.kind with
          | Order.Kind.Market ->
            if left = wanted
            then Order.Rejected Order.Reject.Market_order_no_liquidity
            else Order.Cancelled_remainder (Qty.Pos.of_int_exn left)
          | Order.Kind.Limit px ->
            (match req.tif with
             | Order.Tif.Immediate_or_cancel | Order.Tif.Fill_or_kill ->
               Order.Cancelled_remainder (Qty.Pos.of_int_exn left)
             | Order.Tif.Good_till_cancel ->
               rest t ~req ~qty:left ~level:(level_of_px t px);
               Order.Resting (Qty.Pos.of_int_exn left))))))
;;

let submit t req =
  let acc = ref [] in
  let status =
    submit_iter t req ~on_fill:(fun ~taker ~maker ~price ~qty ->
      acc := { Order.taker; maker; price; qty } :: !acc)
  in
  { Order.fills = List.rev !acc; status }
;;

let cancel t id =
  let slot = Idmap.find t.index (Order.Id.to_int id) in
  if slot = Idmap.not_found
  then `Unknown_id
  else (
    let remaining = t.o_qty.(slot) in
    let is_buy = t.o_is_buy.(slot) in
    let level = t.o_level.(slot) in
    unlink t ~slot;
    let lvl_qty = qtys t ~is_buy in
    lvl_qty.(level) <- lvl_qty.(level) - remaining;
    Idmap.remove t.index (Order.Id.to_int id);
    free_slot t slot;
    t.live <- t.live - 1;
    repair_top t ~is_buy ~level;
    `Cancelled (Qty.Pos.of_int_exn remaining))
;;

(* ------------------------------------------------------------------ *)
(* invariants                                                          *)
(* ------------------------------------------------------------------ *)

let check_invariants t =
  let error fmt = Printf.ksprintf (fun s -> Error s) fmt in
  let queue_sum ~is_buy level =
    let head = (heads t ~is_buy).(level) in
    let rec go slot acc n =
      if slot = null
      then Ok (acc, n)
      else if n > t.capacity
      then error "level %d: queue has a cycle" level
      else go t.o_next.(slot) (acc + t.o_qty.(slot)) (n + 1)
    in
    go head 0 0
  in
  let rec check_levels ~is_buy i =
    if i >= t.n_levels
    then Ok ()
    else (
      match queue_sum ~is_buy i with
      | Error _ as e -> e
      | Ok (sum, count) ->
        let cached = (qtys t ~is_buy).(i) in
        if sum <> cached
        then
          error
            "level %d (%s): cached quantity %d but queue holds %d"
            i
            (if is_buy then "bid" else "ask")
            cached
            sum
        else if count > 0 && cached = 0
        then error "level %d: non-empty queue with zero cached quantity" i
        else (
          (* Time priority: sequence numbers must increase along the queue. *)
          let head = (heads t ~is_buy).(i) in
          let rec monotone slot last =
            if slot = null
            then Ok ()
            else if t.o_seq.(slot) <= last
            then error "level %d: queue is out of arrival order" i
            else monotone t.o_next.(slot) t.o_seq.(slot)
          in
          match monotone head (-1) with
          | Error _ as e -> e
          | Ok () -> check_levels ~is_buy (i + 1)))
  in
  match check_levels ~is_buy:true 0 with
  | Error _ as e -> e
  | Ok () ->
    (match check_levels ~is_buy:false 0 with
     | Error _ as e -> e
     | Ok () ->
       let bid = scan_down t.bid_qty (t.n_levels - 1) in
       let ask = scan_up t.ask_qty t.n_levels 0 in
       if bid <> t.best_bid
       then error "cached best bid %d but true best bid is %d" t.best_bid bid
       else if ask <> t.best_ask
       then error "cached best ask %d but true best ask is %d" t.best_ask ask
       else if bid <> null && ask < t.n_levels && bid >= ask
       then error "book is crossed: best bid %d >= best ask %d" bid ask
       else if Idmap.length t.index <> t.live
       then
         error
           "index holds %d orders but live count is %d"
           (Idmap.length t.index)
           t.live
       else Ok ())
;;
