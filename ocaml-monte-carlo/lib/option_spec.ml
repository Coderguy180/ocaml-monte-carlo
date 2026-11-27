(* What we are pricing. Payoffs are a closed sum type, so the compiler
   forces every pricing function to handle every kind of contract. *)

type side = Call | Put

type barrier =
  | Up_and_out of float
  | Down_and_out of float

type style =
  | European
  | Asian_arithmetic        (* payoff on the average price along the path *)
  | Barrier of barrier      (* knocked out if the path touches the level  *)

type t = {
  side : side;
  style : style;
  strike : float;
  expiry : float;           (* in years *)
}

type market = {
  spot : float;
  rate : float;             (* continuously compounded risk-free rate *)
  vol : float;              (* annualised volatility *)
}

let intrinsic side ~strike s =
  match side with
  | Call -> Float.max (s -. strike) 0.0
  | Put -> Float.max (strike -. s) 0.0

let is_path_dependent t =
  match t.style with
  | European -> false
  | Asian_arithmetic | Barrier _ -> true

let side_to_string = function Call -> "call" | Put -> "put"

let to_string t =
  let style =
    match t.style with
    | European -> "European"
    | Asian_arithmetic -> "Asian (arithmetic)"
    | Barrier (Up_and_out b) -> Printf.sprintf "Up-and-out barrier @ %.1f" b
    | Barrier (Down_and_out b) -> Printf.sprintf "Down-and-out barrier @ %.1f" b
  in
  Printf.sprintf "%s %s, K=%.1f, T=%.2fy" style (side_to_string t.side)
    t.strike t.expiry
