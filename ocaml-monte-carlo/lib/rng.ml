(* Standard normal sampling via the Marsaglia polar method.

   The polar method produces normals in pairs, so we cache the spare one.
   The state is explicit (no global RNG) so every simulation is reproducible
   from its seed and independent streams can't interfere with each other. *)

type t = {
  state : Random.State.t;
  mutable spare : float option;
}

let create ~seed = { state = Random.State.make [| seed |]; spare = None }

let uniform t = Random.State.float t.state 1.0

let rec gaussian t =
  match t.spare with
  | Some z ->
    t.spare <- None;
    z
  | None ->
    let u = (2.0 *. uniform t) -. 1.0 in
    let v = (2.0 *. uniform t) -. 1.0 in
    let s = (u *. u) +. (v *. v) in
    if s >= 1.0 || s = 0.0 then gaussian t
    else begin
      let m = sqrt (-2.0 *. log s /. s) in
      t.spare <- Some (v *. m);
      u *. m
    end
