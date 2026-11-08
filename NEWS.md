# copom (development version)

* `interpolate()` on `FlatForwardCOPOM` and `COPOMScenarios` interpolations
  now returns `numeric` instead of a `Term` object (#1).
* `get_copom_dates()` includes the meetings of 2025, 2026 and 2027, and has a
  new `guesses` argument to add dates that BCB has not announced yet.
* The vignettes are built from saved data, so they no longer need web access
  (#7).
* The package now depends on R >= 4.1.0 (it uses the native pipe `|>`).

# copom 0.0.2

* `get_copom_dates` updated with dates of 2024

# copom 0.0.1

* First release
