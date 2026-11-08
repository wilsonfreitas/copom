# Downloads the data used by the vignettes, so they build without web access.
# Run from the package root: Rscript data-raw/vignette-data.R

focus <- rbcb::get_market_expectations("top5s-selic",
  start_date = as.Date("2022-04-29"),
  end_date = as.Date("2022-04-29")
)
write.csv(focus, "vignettes/focus-selic-2022-04-29.csv", row.names = FALSE)

selic <- rbcb::get_series(c(SELIC = 432),
  start_date = as.Date("2021-02-01"),
  end_date = "2022-02-23"
)
write.csv(selic, "vignettes/selic-2021-02-01-2022-02-23.csv", row.names = FALSE)
