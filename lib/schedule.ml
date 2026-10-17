open Base

let kappa_tilde ~kappa ~tau =
  if Float.( <= ) (Float.abs kappa) 1e-12
  then 0.
  else (
    (* Solve 2 (cosh (k~ tau) - 1) / tau^2 = kappa^2 for k~, exactly:
       cosh x - 1 = 2 sinh^2 (x/2), so sinh (k~ tau / 2) = kappa tau / 2. *)
    let s = kappa *. tau /. 2. in
    2. /. tau *. Float.log (s +. Float.sqrt ((s *. s) +. 1.)))
;;

let sinh_path ~total ~slices ~kappa ~horizon =
  if slices <= 0 then invalid_arg "Schedule.sinh_path: slices must be positive";
  let tau = horizon /. Float.of_int slices in
  let path = Array.create ~len:(slices + 1) 0. in
  if Float.( <= ) (Float.abs kappa) 1e-12
  then
    (* Risk-neutral: trade at a constant rate. *)
    for k = 0 to slices do
      path.(k) <- total *. (1. -. (Float.of_int k /. Float.of_int slices))
    done
  else (
    let kt = kappa_tilde ~kappa ~tau in
    let denom = Float.sinh (kt *. horizon) in
    for k = 0 to slices do
      let t = Float.of_int k *. tau in
      path.(k) <- total *. Float.sinh (kt *. (horizon -. t)) /. denom
    done);
  path.(slices) <- 0.;
  path
;;

(* Stationarity of the quadratic objective with respect to the interior
   holdings gives, for k = 1 .. N-1,

     -x_{k-1} + (2 + lambda sigma_k^2 tau^2 / eta) x_k - x_{k+1} = 0,

   with x_0 = total and x_N = 0 fixed. The matrix is tridiagonal, symmetric and
   diagonally dominant (the diagonal exceeds 2 whenever sigma > 0), so the
   Thomas algorithm is stable here without pivoting and runs in O(N). *)
let variable_vol_path ~total ~sigma ~risk_aversion ~eta ~horizon =
  let n = Array.length sigma in
  if n <= 0 then invalid_arg "Schedule.variable_vol_path: need at least one slice";
  if Float.( <= ) eta 0.
  then invalid_arg "Schedule.variable_vol_path: eta must be positive";
  let path = Array.create ~len:(n + 1) 0. in
  path.(0) <- total;
  if n = 1
  then path
  else (
    let tau = horizon /. Float.of_int n in
    let m = n - 1 in
    (* interior unknowns x_1 .. x_{n-1} *)
    let diag = Array.create ~len:m 0. in
    let rhs = Array.create ~len:m 0. in
    for i = 0 to m - 1 do
      let k = i + 1 in
      let s = sigma.(k) in
      diag.(i) <- 2. +. (risk_aversion *. s *. s *. tau *. tau /. eta);
      rhs.(i) <- 0.
    done;
    rhs.(0) <- total;
    (* x_0 moves to the right-hand side *)
    (* x_n = 0, so the last row needs no adjustment *)
    (* Thomas: forward elimination with off-diagonals fixed at -1. *)
    let c' = Array.create ~len:m 0. in
    let d' = Array.create ~len:m 0. in
    c'.(0) <- -1. /. diag.(0);
    d'.(0) <- rhs.(0) /. diag.(0);
    for i = 1 to m - 1 do
      let denom = diag.(i) +. c'.(i - 1) in
      c'.(i) <- -1. /. denom;
      d'.(i) <- (rhs.(i) +. d'.(i - 1)) /. denom
    done;
    (* back substitution *)
    let x = Array.create ~len:m 0. in
    x.(m - 1) <- d'.(m - 1);
    for i = m - 2 downto 0 do
      x.(i) <- d'.(i) -. (c'.(i) *. x.(i + 1))
    done;
    Array.blit ~src:x ~src_pos:0 ~dst:path ~dst_pos:1 ~len:m;
    path.(n) <- 0.;
    path)
;;

let to_trades path =
  let n = Array.length path - 1 in
  Array.init n ~f:(fun k -> path.(k) -. path.(k + 1))
;;

let objective ~path ~sigma ~risk_aversion ~eta ~horizon =
  let n = Array.length path - 1 in
  let tau = horizon /. Float.of_int n in
  let impact = ref 0. in
  for k = 1 to n do
    let traded = path.(k - 1) -. path.(k) in
    impact := !impact +. (eta *. traded *. traded /. tau)
  done;
  let risk = ref 0. in
  for k = 1 to n - 1 do
    let s = if k < Array.length sigma then sigma.(k) else sigma.(Array.length sigma - 1) in
    risk := !risk +. (risk_aversion *. s *. s *. tau *. path.(k) *. path.(k))
  done;
  !impact +. !risk
;;
