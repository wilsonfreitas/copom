# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this is

R package `copom` (repo dir is `flatforwardCOPOM`). It adds interest-rate curve interpolations that are flat between Brazilian COPOM meetings instead of flat between bond maturities (method from Carreira & Brostowicz, *Brazilian Derivatives and Securities*, 2016). It plugs into the `fixedincome` package (`wilsonfreitas/R-fixedincome`, installed from GitHub via `Remotes`) and uses `bizdays` with the `"Brazil/ANBIMA"` calendar.

## Commands

```r
devtools::document()      # regenerate NAMESPACE and man/*.Rd from roxygen comments
devtools::load_all()      # load package for interactive work
devtools::check()         # R CMD check (runs roxygen @examples)
devtools::build_readme()  # README.md is generated from README.Rmd — edit the .Rmd
pkgdown::build_site()     # docs site (also built by .github/workflows/pkgdown.yaml)
```

There is no `tests/` directory. Verification is via roxygen `@examples` (run by `check()`) and the scripts in `examples/` (excluded from the build; some fetch data from the web via `examples/utils-functions.R`).

## Architecture

- Both interpolations are S4 classes extending `fixedincome`'s `Interpolation` and implement its generics (`prepare_interpolation`, `fit_interpolation`) for `SpotRateCurve`. Users set them with `interpolation(curve) <- interp_flatforwardcopom(...)`; `fixedincome` then calls `prepare_interpolation`, which must fill `object@func` (term in days → spot rate).
- Both build `func` the same way: linear interpolation of `log(compound(...))` (log discount factors) over terms, then `implied_rate` back to a rate.
- **`FlatForwardCOPOM`** (`R/flatforwardcopom-class.R` + helpers in `R/flatforwardcopom-functions.R`): `split_curve_into_copom_dates` groups curve vertices by the meeting interval they fall in; `copom_calc` walks the intervals recursively, building the zero rate at each meeting date from the previous result (`calc_zero`) and the forward after it via a strategy chosen by `conflicts` (`forward`, `first`, `second`, `optimize`) when an interval has 2+ futures. Implied per-meeting moves are stored in `object@moves`.
- **`COPOMScenarios`** (`R/copomscenarios-class.R`): builds the curve from user scenarios (`copom_moves` *or* `future_rates`, in decimals, not bps). `fit_interpolation` uses `optim` (BFGS) to find moves that minimize `interpolation_error`.
- `get_copom_dates()` (`R/copom-dates.R`) is a hard-coded list of meeting dates; extend it by hand when new dates are announced.
- `DESCRIPTION` has a `Collate:` field and files use `@include`; keep the order when adding files. All `fixedincome`/`stats` imports are declared in `R/copom-package.R`.
