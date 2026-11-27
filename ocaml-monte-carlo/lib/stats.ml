(* Numerically stable streaming mean/variance (Welford's algorithm).
   Constant memory, so we never need to keep millions of samples around. *)

type t = {
  mutable n : int;
  mutable mean : float;
  mutable m2 : float;
}

let create () = { n = 0; mean = 0.0; m2 = 0.0 }

let add t x =
  t.n <- t.n + 1;
  let delta = x -. t.mean in
  t.mean <- t.mean +. (delta /. float_of_int t.n);
  t.m2 <- t.m2 +. (delta *. (x -. t.mean))

let count t = t.n
let mean t = t.mean

let variance t =
  if t.n < 2 then nan else t.m2 /. float_of_int (t.n - 1)

let std_error t = sqrt (variance t /. float_of_int t.n)

(* Sample covariance of two paired streams; used for control variates. *)
module Pair = struct
  type t = {
    mutable n : int;
    mutable mean_x : float;
    mutable mean_y : float;
    mutable cxy : float;
    mutable m2x : float;
  }

  let create () = { n = 0; mean_x = 0.0; mean_y = 0.0; cxy = 0.0; m2x = 0.0 }

  let add t x y =
    t.n <- t.n + 1;
    let n = float_of_int t.n in
    let dx = x -. t.mean_x in
    t.mean_x <- t.mean_x +. (dx /. n);
    t.mean_y <- t.mean_y +. ((y -. t.mean_y) /. n);
    t.cxy <- t.cxy +. (dx *. (y -. t.mean_y));
    t.m2x <- t.m2x +. (dx *. (x -. t.mean_x))

  (* Optimal control-variate coefficient: Cov(X, Y) / Var(X). *)
  let beta t = if t.m2x = 0.0 then 0.0 else t.cxy /. t.m2x
end
