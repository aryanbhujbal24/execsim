open Base
open Execsim
let p = Stdlib.print_endline
let words n f =
  let b = Stdlib.Gc.minor_words () in f ();
  (Stdlib.Gc.minor_words () -. b) /. Float.of_int n
let n = 200_000
let () =
  let sink = ref 0 in
  p (Printf.sprintf "Order.limit record         %6.2f w/op"
    (words n (fun () -> for i = 1 to n do
      let r = Order.limit ~id:i ~side:Buy ~px:9999 ~qty:10 () in
      sink := !sink + Qty.Pos.to_int r.qty done)));
  let b = Book.create ~floor:0 ~ceiling:20000 ~capacity:(n+16) () in
  p (Printf.sprintf "submit_iter (rest)         %6.2f w/op"
    (words n (fun () -> for i = 1 to n do
      ignore (Book.submit_iter b (Order.limit ~id:i ~side:Buy ~px:(9999 - i % 300) ~qty:10 ())
        ~on_fill:(fun ~taker:_ ~maker:_ ~price:_ ~qty:_ -> ()) : Order.status) done)));
  p (Printf.sprintf "Book.cancel                %6.2f w/op"
    (words n (fun () -> for i = 1 to n do
      match Book.cancel b (Order.Id.of_int i) with `Cancelled _ | `Unknown_id -> () done)));
  (* matching path *)
  let b2 = Book.create ~floor:0 ~ceiling:20000 ~capacity:(n+16) () in
  for i = 1 to n do
    ignore (Book.submit_iter b2 (Order.limit ~id:i ~side:Sell ~px:(10001 + i % 200) ~qty:10 ())
      ~on_fill:(fun ~taker:_ ~maker:_ ~price:_ ~qty:_ -> ()) : Order.status) done;
  let fills = ref 0 in
  p (Printf.sprintf "submit_iter (one fill)     %6.2f w/op"
    (words n (fun () -> for i = 1 to n do
      ignore (Book.submit_iter b2 (Order.market ~id:(5_000_000+i) ~side:Buy ~qty:10 ())
        ~on_fill:(fun ~taker:_ ~maker:_ ~price:_ ~qty:_ -> Int.incr fills) : Order.status) done)));
  let b3 = Book.create ~floor:0 ~ceiling:20000 ~capacity:(n+16) () in
  for i = 1 to n do
    ignore (Book.submit_iter b3 (Order.limit ~id:i ~side:Sell ~px:(10001 + i % 200) ~qty:10 ())
      ~on_fill:(fun ~taker:_ ~maker:_ ~price:_ ~qty:_ -> ()) : Order.status) done;
  p (Printf.sprintf "submit (builds fill list)  %6.2f w/op"
    (words n (fun () -> for i = 1 to n do
      ignore (Book.submit b3 (Order.market ~id:(6_000_000+i) ~side:Buy ~qty:10 ())
        : Order.outcome) done)));
  p (Printf.sprintf "  (sink %d, fills %d)" !sink !fills)
