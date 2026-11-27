(* Statistical tests: a Monte Carlo price is only "correct" up to noise,
   so we assert it lands within a few standard errors of the truth. *)

open Montecarlo
open Option_spec

let failures = ref 0

let check name cond =
  if cond then Printf.printf "PASS  %s\n" name
  else begin
    incr failures;
    Printf.printf "FAIL  %s\n" name
  end

let mkt = { spot = 100.0; rate = 0.05; vol = 0.20 }
let config = { Engine.default_config with paths = 400_000 }

let within_4se (r : Engine.result) truth =
  Float.abs (r.price -. truth) < 4.0 *. r.std_error

let () =
  (* European call and put match Black-Scholes across strikes *)
  List.iter
    (fun strike ->
       let bs_c =
         Black_scholes.call ~spot:mkt.spot ~strike ~rate:mkt.rate ~vol:mkt.vol
           ~expiry:1.0
       in
       let bs_p =
         Black_scholes.put ~spot:mkt.spot ~strike ~rate:mkt.rate ~vol:mkt.vol
           ~expiry:1.0
       in
       let c =
         Engine.price ~config
           { side = Call; style = European; strike; expiry = 1.0 } mkt
       in
       let p =
         Engine.price ~config
           { side = Put; style = European; strike; expiry = 1.0 } mkt
       in
       check (Printf.sprintf "call K=%.0f matches BS" strike) (within_4se c bs_c);
       check (Printf.sprintf "put  K=%.0f matches BS" strike) (within_4se p bs_p))
    [ 80.0; 100.0; 120.0 ];

  (* Put-call parity holds for the closed form *)
  let c = Black_scholes.call ~spot:100.0 ~strike:105.0 ~rate:0.05 ~vol:0.3 ~expiry:0.5 in
  let p = Black_scholes.put ~spot:100.0 ~strike:105.0 ~rate:0.05 ~vol:0.3 ~expiry:0.5 in
  check "put-call parity"
    (Float.abs (c -. p -. (100.0 -. (105.0 *. exp (-0.025)))) < 1e-10);

  (* Variance reduction actually reduces variance *)
  let opt = { side = Call; style = European; strike = 100.0; expiry = 1.0 } in
  let plain =
    Engine.price
      ~config:{ config with antithetic = false; control_variate = false } opt mkt
  in
  let reduced = Engine.price ~config opt mkt in
  check "variance reduction lowers std error"
    (reduced.std_error < plain.std_error /. 2.0);

  (* Same seed -> identical result *)
  let a = Engine.price ~config opt mkt and b = Engine.price ~config opt mkt in
  check "deterministic given seed" (a.price = b.price);

  (* No-arbitrage bounds on exotics *)
  let euro = Engine.price ~config opt mkt in
  let asian = Engine.price ~config { opt with style = Asian_arithmetic } mkt in
  let ko =
    Engine.price ~config { opt with style = Barrier (Up_and_out 130.0) } mkt
  in
  check "Asian call < European call" (asian.price < euro.price);
  check "knock-out call < European call" (ko.price < euro.price);
  check "knock-out call >= 0" (ko.price >= 0.0);
  let far =
    Engine.price ~config { opt with style = Barrier (Up_and_out 1e9) } mkt
  in
  check "unreachable barrier ~ European" (within_4se far euro.price);

  if !failures > 0 then begin
    Printf.printf "\n%d test(s) failed\n" !failures;
    exit 1
  end
  else print_endline "\nAll tests passed"
