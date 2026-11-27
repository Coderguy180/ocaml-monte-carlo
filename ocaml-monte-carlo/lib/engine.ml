(* Monte Carlo pricing engine.

   The underlying follows geometric Brownian motion under the risk-neutral
   measure:   dS = r S dt + sigma S dW
   We use the exact log-space discretisation, so there is no time-stepping
   bias for European payoffs:
     S(t + dt) = S(t) * exp((r - sigma^2/2) dt + sigma sqrt(dt) Z)

   Two variance-reduction techniques can be layered on top:
   - Antithetic variates: each draw Z is paired with -Z. The two payoffs are
     negatively correlated, so their average has lower variance.
   - Control variate: the discounted terminal price S(T) has a known mean
     (S0, since discounted prices are martingales). We regress the payoff on
     it and subtract the predictable part:  Y - beta * (X - E[X]).          *)

open Option_spec

type config = {
  paths : int;
  steps : int;              (* time steps per path (path-dependent only) *)
  antithetic : bool;
  control_variate : bool;
  seed : int;
}

let default_config =
  { paths = 100_000; steps = 252; antithetic = true;
    control_variate = true; seed = 42 }

type result = {
  price : float;
  std_error : float;
  ci_low : float;           (* 95% confidence interval *)
  ci_high : float;
  samples : int;
}

(* Simulate one path from a pre-drawn array of normals. Returns the payoff
   and the terminal price. [sign] flips the shocks for the antithetic twin,
   letting both legs reuse the same array without re-allocation. *)
let simulate_path opt mkt ~steps ~normals ~sign =
  let dt = opt.expiry /. float_of_int steps in
  let drift = (mkt.rate -. (0.5 *. mkt.vol *. mkt.vol)) *. dt in
  let diffusion = mkt.vol *. sqrt dt in
  let breached s =
    match opt.style with
    | Barrier (Up_and_out b) -> s >= b
    | Barrier (Down_and_out b) -> s <= b
    | European | Asian_arithmetic -> false
  in
  let rec go i s sum alive =
    if i = steps then (s, sum, alive)
    else begin
      let s' = s *. exp (drift +. (diffusion *. sign *. normals.(i))) in
      let alive = alive && not (breached s') in
      go (i + 1) s' (sum +. s') alive
    end
  in
  let s_t, sum, alive = go 0 mkt.spot 0.0 true in
  let payoff =
    match opt.style with
    | European -> intrinsic opt.side ~strike:opt.strike s_t
    | Asian_arithmetic ->
      intrinsic opt.side ~strike:opt.strike (sum /. float_of_int steps)
    | Barrier _ ->
      if alive then intrinsic opt.side ~strike:opt.strike s_t else 0.0
  in
  (payoff, s_t)

let price ?(config = default_config) opt mkt =
  if config.paths < 2 then invalid_arg "Engine.price: need at least 2 paths";
  (* A European payoff only depends on S(T): one step is exact. *)
  let steps = if is_path_dependent opt then config.steps else 1 in
  let rng = Rng.create ~seed:config.seed in
  let discount = exp (-.mkt.rate *. opt.expiry) in
  let normals = Array.make steps 0.0 in
  (* Each "sample" is one draw (or the average of an antithetic pair).
     Pairs keep samples i.i.d., which keeps the standard error honest. *)
  let n_samples =
    if config.antithetic then max 2 (config.paths / 2) else config.paths
  in
  let ys = Array.make n_samples 0.0 in
  let xs = Array.make n_samples 0.0 in
  for k = 0 to n_samples - 1 do
    for i = 0 to steps - 1 do
      normals.(i) <- Rng.gaussian rng
    done;
    let p1, s1 = simulate_path opt mkt ~steps ~normals ~sign:1.0 in
    let y, x =
      if config.antithetic then begin
        let p2, s2 = simulate_path opt mkt ~steps ~normals ~sign:(-1.0) in
        (0.5 *. (p1 +. p2), 0.5 *. (s1 +. s2))
      end
      else (p1, s1)
    in
    ys.(k) <- discount *. y;
    xs.(k) <- discount *. x
  done;
  let stats = Stats.create () in
  if config.control_variate then begin
    let pair = Stats.Pair.create () in
    Array.iteri (fun k x -> Stats.Pair.add pair x ys.(k)) xs;
    let beta = Stats.Pair.beta pair in
    (* E[discounted S(T)] = S0 under the risk-neutral measure. *)
    Array.iteri
      (fun k y -> Stats.add stats (y -. (beta *. (xs.(k) -. mkt.spot))))
      ys
  end
  else Array.iter (Stats.add stats) ys;
  let price = Stats.mean stats in
  let se = Stats.std_error stats in
  { price; std_error = se; ci_low = price -. (1.96 *. se);
    ci_high = price +. (1.96 *. se); samples = Stats.count stats }

(* Pathwise-free Greeks by central finite differences. Reusing the same
   seed for the bumped runs (common random numbers) cancels most of the
   noise, which is what makes finite differences on MC usable at all. *)
let delta ?(config = default_config) ?(bump = 0.01) opt mkt =
  let h = mkt.spot *. bump in
  let up = price ~config opt { mkt with spot = mkt.spot +. h } in
  let dn = price ~config opt { mkt with spot = mkt.spot -. h } in
  (up.price -. dn.price) /. (2.0 *. h)

let vega ?(config = default_config) ?(bump = 0.01) opt mkt =
  let up = price ~config opt { mkt with vol = mkt.vol +. bump } in
  let dn = price ~config opt { mkt with vol = mkt.vol -. bump } in
  (up.price -. dn.price) /. (2.0 *. bump)
