(* Demo: validate the engine against Black-Scholes, show how variance
   reduction and path count affect the error, then price exotics. *)

open Montecarlo
open Option_spec

let mkt = { spot = 100.0; rate = 0.05; vol = 0.20 }

let european side = { side; style = European; strike = 100.0; expiry = 1.0 }

let time f =
  let t0 = Sys.time () in
  let r = f () in
  (r, Sys.time () -. t0)

let print_result ?(width = 28) label (r : Engine.result) secs =
  Printf.printf "  %-*s %9.4f  ± %.4f   [%.4f, %.4f]  %6.2fs\n" width label
    r.price r.std_error r.ci_low r.ci_high secs

let section title = Printf.printf "\n== %s ==\n" title

let () =
  Printf.printf "Market: S0=%.1f  r=%.2f%%  sigma=%.0f%%\n" mkt.spot
    (100.0 *. mkt.rate) (100.0 *. mkt.vol);

  (* 1. Correctness: MC vs closed form *)
  section "European call vs Black-Scholes (1M paths)";
  let opt = european Call in
  let bs =
    Black_scholes.call ~spot:mkt.spot ~strike:opt.strike ~rate:mkt.rate
      ~vol:mkt.vol ~expiry:opt.expiry
  in
  Printf.printf "  %-28s %9.4f\n" "Black-Scholes (exact)" bs;
  let variants =
    [ ("Plain MC", false, false);
      ("Antithetic", true, false);
      ("Control variate", false, true);
      ("Antithetic + control", true, true) ]
  in
  let base_se = ref nan in
  List.iter
    (fun (label, antithetic, control_variate) ->
       let config =
         { Engine.default_config with paths = 1_000_000; antithetic;
                                      control_variate }
       in
       let r, secs = time (fun () -> Engine.price ~config opt mkt) in
       if Float.is_nan !base_se then base_se := r.std_error;
       print_result label r secs;
       Printf.printf "  %-28s variance reduced %.1fx, error vs BS %+.4f\n" ""
         ((!base_se /. r.std_error) ** 2.0) (r.price -. bs))
    variants;

  (* 2. Convergence: error shrinks like 1/sqrt(N) *)
  section "Convergence of plain MC (std error ~ 1/sqrt N)";
  Printf.printf "  %10s %12s %12s %14s\n" "paths" "price" "std err"
    "se * sqrt(N)";
  List.iter
    (fun paths ->
       let config =
         { Engine.default_config with paths; antithetic = false;
                                      control_variate = false }
       in
       let r = Engine.price ~config opt mkt in
       Printf.printf "  %10d %12.4f %12.5f %14.3f\n" paths r.price r.std_error
         (r.std_error *. sqrt (float_of_int paths)))
    [ 1_000; 10_000; 100_000; 1_000_000 ];

  (* 3. Greeks *)
  section "Greeks (finite differences, common random numbers)";
  let config = { Engine.default_config with paths = 200_000 } in
  let d1, _ =
    Black_scholes.d1_d2 ~spot:mkt.spot ~strike:opt.strike ~rate:mkt.rate
      ~vol:mkt.vol ~expiry:opt.expiry
  in
  let bs_delta = Black_scholes.norm_cdf d1 in
  let bs_vega =
    mkt.spot *. sqrt opt.expiry *. exp (-0.5 *. d1 *. d1) /. sqrt (2.0 *. Float.pi)
  in
  Printf.printf "  delta  MC %.4f   BS %.4f\n" (Engine.delta ~config opt mkt)
    bs_delta;
  Printf.printf "  vega   MC %.4f  BS %.4f\n" (Engine.vega ~config opt mkt)
    bs_vega;

  (* 4. Exotics, where there is no simple closed form *)
  section "Path-dependent options (200k paths, 252 steps)";
  let exotics =
    [ { side = Call; style = Asian_arithmetic; strike = 100.0; expiry = 1.0 };
      { side = Call; style = Barrier (Up_and_out 130.0); strike = 100.0;
        expiry = 1.0 };
      { side = Put; style = Barrier (Down_and_out 80.0); strike = 100.0;
        expiry = 1.0 } ]
  in
  List.iter
    (fun opt ->
       let r, secs = time (fun () -> Engine.price ~config opt mkt) in
       print_result ~width:50 (to_string opt) r secs)
    exotics
