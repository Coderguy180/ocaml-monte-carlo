# Monte Carlo Option Pricer (OCaml)

A Monte Carlo engine for pricing equity options under geometric Brownian
motion, written in OCaml using only the standard library.

## Features

- **Exact GBM discretisation** in log space, so European prices have no time-stepping bias
- **Contracts:** European, arithmetic Asian, and up/down-and-out barrier options (calls and puts), modelled as a variant type so the compiler checks every pricing path handles every contract
- **Variance reduction:** antithetic variates and a control variate (discounted terminal price, regression-optimal β). Together they cut variance by **~29x** on an ATM call
- **Statistics:** Welford streaming mean and variance, standard errors, 95% confidence intervals
- **Greeks:** delta and vega by central finite differences with common random numbers
- **Validation:** checked against closed-form Black-Scholes, plus statistical tests (within 4σ), no-arbitrage bounds and reproducibility checks
- Explicit, seeded RNG state (Marsaglia polar method), so runs are fully reproducible

## Run

```bash
dune build
dune test                 # statistical test suite
dune exec bin/main.exe    # demo: validation, convergence, Greeks, exotics
```

## Layout

| File | Purpose |
|---|---|
| `lib/rng.ml` | Seeded Gaussian sampler |
| `lib/stats.ml` | Streaming mean/variance and covariance |
| `lib/black_scholes.ml` | Closed-form benchmark |
| `lib/option_spec.ml` | Contract and market types |
| `lib/engine.ml` | Path simulation, variance reduction, Greeks |
| `bin/main.ml` | Demo |
| `test/` | Tests |
