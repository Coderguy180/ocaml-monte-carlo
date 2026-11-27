(* Closed-form Black-Scholes prices: the ground truth for validating the
   simulator, and the control variate for the path-dependent options. *)

let norm_cdf x = 0.5 *. (1.0 +. Float.erf (x /. sqrt 2.0))

let d1_d2 ~spot ~strike ~rate ~vol ~expiry =
  let sqrt_t = sqrt expiry in
  let d1 =
    (log (spot /. strike) +. ((rate +. (0.5 *. vol *. vol)) *. expiry))
    /. (vol *. sqrt_t)
  in
  (d1, d1 -. (vol *. sqrt_t))

let call ~spot ~strike ~rate ~vol ~expiry =
  let d1, d2 = d1_d2 ~spot ~strike ~rate ~vol ~expiry in
  (spot *. norm_cdf d1) -. (strike *. exp (-.rate *. expiry) *. norm_cdf d2)

let put ~spot ~strike ~rate ~vol ~expiry =
  let d1, d2 = d1_d2 ~spot ~strike ~rate ~vol ~expiry in
  (strike *. exp (-.rate *. expiry) *. norm_cdf (-.d2))
  -. (spot *. norm_cdf (-.d1))
