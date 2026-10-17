open Base

(* Two key values are reserved as markers. Order ids in practice are positive,
   so reserving the two most negative integers costs nothing. *)
let empty_key = Int.min_value
let tomb_key = Int.min_value + 1
let not_found = Int.min_value

type t =
  { mutable keys : int array
  ; mutable vals : int array
  ; mutable mask : int (* capacity - 1; capacity is a power of two *)
  ; mutable count : int (* live entries *)
  ; mutable used : int (* live entries plus tombstones *)
  }

let rec next_pow2 n acc = if acc >= n then acc else next_pow2 n (acc * 2)

let create n =
  let cap = next_pow2 (Int.max 8 (n * 2)) 8 in
  { keys = Array.create ~len:cap empty_key
  ; vals = Array.create ~len:cap 0
  ; mask = cap - 1
  ; count = 0
  ; used = 0
  }
;;

(* Multiplicative hashing with an odd mixing constant, then a xor-shift to
   drive the high bits down into the low ones that the mask selects. Order ids
   are usually sequential, and sequential keys under a weak hash cluster badly
   with linear probing. The constant is kept inside OCaml's 63-bit int. *)
let hash key =
  let h = key * 0x2545F4914F6CDD1D in
  let h = h lxor (h lsr 29) in
  h land Int.max_value
;;

(* The probe loops below are top-level functions taking every piece of state as
   an argument, rather than the more natural [let rec] nested inside [find] and
   friends. A nested recursive function captures its environment, and without
   flambda the compiler allocates a closure for it on every call — measured at
   seven words per lookup, which is the entire allocation budget of a cancel.
   Hoisting them out makes the hot path allocate nothing at all. *)

let rec probe_find keys vals mask key i =
  let k = Array.unsafe_get keys i in
  if k = key
  then Array.unsafe_get vals i
  else if k = empty_key
  then not_found
  else probe_find keys vals mask key ((i + 1) land mask)
;;

let find t key = probe_find t.keys t.vals t.mask key (hash key land t.mask)
let mem t key = find t key <> not_found

(* Returns the slot to write, or [-1] if the key was already present and
   updated in place. [first_tomb] lets an insert reclaim a tombstone it passed
   rather than extending the probe run. *)
let rec probe_set keys vals mask key data i first_tomb =
  let k = Array.unsafe_get keys i in
  if k = key
  then (
    Array.unsafe_set vals i data;
    -1)
  else if k = empty_key
  then
    if first_tomb >= 0
    then (
      Array.unsafe_set keys first_tomb key;
      Array.unsafe_set vals first_tomb data;
      -2 (* reused a tombstone: live count rises, used count does not *))
    else (
      Array.unsafe_set keys i key;
      Array.unsafe_set vals i data;
      i)
  else if k = tomb_key
  then
    probe_set keys vals mask key data ((i + 1) land mask)
      (if first_tomb >= 0 then first_tomb else i)
  else probe_set keys vals mask key data ((i + 1) land mask) first_tomb
;;

let rec probe_remove keys mask key i =
  let k = Array.unsafe_get keys i in
  if k = key
  then (
    Array.unsafe_set keys i tomb_key;
    true)
  else if k = empty_key
  then false
  else probe_remove keys mask key ((i + 1) land mask)
;;

let rec reinsert keys vals mask key data i =
  if Array.unsafe_get keys i = empty_key
  then (
    Array.unsafe_set keys i key;
    Array.unsafe_set vals i data)
  else reinsert keys vals mask key data ((i + 1) land mask)
;;

let rebuild t new_cap =
  let keys = Array.create ~len:new_cap empty_key in
  let vals = Array.create ~len:new_cap 0 in
  let mask = new_cap - 1 in
  let old_keys = t.keys
  and old_vals = t.vals in
  for i = 0 to Array.length old_keys - 1 do
    let k = old_keys.(i) in
    if k <> empty_key && k <> tomb_key
    then reinsert keys vals mask k old_vals.(i) (hash k land mask)
  done;
  t.keys <- keys;
  t.vals <- vals;
  t.mask <- mask;
  t.used <- t.count
;;

let set t ~key ~data =
  if key = empty_key || key = tomb_key then invalid_arg "Idmap: reserved key";
  (* Keep the load factor under one half: linear probing degrades sharply above
     that. Tombstones count toward the load, so a workload of pure insert and
     remove still triggers a periodic rebuild rather than degenerating into a
     linear scan. The rebuild only grows the table when live entries actually
     warrant it; otherwise it rebuilds at the same size, sweeping tombstones. *)
  if (t.used + 1) * 2 > t.mask + 1
  then rebuild t (if t.count * 4 > t.mask + 1 then (t.mask + 1) * 2 else t.mask + 1);
  let slot =
    probe_set t.keys t.vals t.mask key data (hash key land t.mask) (-1)
  in
  if slot = -2
  then t.count <- t.count + 1
  else if slot >= 0
  then (
    t.count <- t.count + 1;
    t.used <- t.used + 1)
;;

let remove t key =
  if probe_remove t.keys t.mask key (hash key land t.mask) then t.count <- t.count - 1
;;

let length t = t.count

let clear t =
  Array.fill t.keys ~pos:0 ~len:(Array.length t.keys) empty_key;
  t.count <- 0;
  t.used <- 0
;;
