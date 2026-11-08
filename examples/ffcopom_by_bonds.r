source("examples/utils-functions.R")

refdate <- as.Date("2022-09-05")
fetch_marketdata("b3-futures-settlement-prices", refdate = refdate)

futures <- futures_get() |>
  filter(refdate == !!refdate, commodity == "DI1") |>
  collect() |>
  mutate(
    maturity_date = maturitycode2date(maturity_code) |>
      following("Brazil/ANBIMA"),
    business_days = bizdays(refdate, maturity_date, "Brazil/ANBIMA"),
    notional = 100000,
    spot_price = price
  ) |>
  filter(business_days > 0, business_days <= 252) |>
  select(refdate, symbol, maturity_date, notional, business_days, spot_price)

fit_interpolation_with_bonds <- function(object, x, bonds,
                                         limits = c(-0.02, 0.02)) {
  par <- object@copom_moves

  fn <- function(par, x, .dates) {
    interpolation(x) <- interp_copomscenarios(.dates, par)
    o <- bonds$spot_price - bonds$notional * discount(x[[bonds$business_days]])
    sqrt(sum(o^2, na.rm = TRUE))
  }

  res <- optim(par, fn,
    method = "L-BFGS-B",
    lower = limits[1], upper = limits[2],
    control = list(maxit = 1000, trace = TRUE),
    x = x, .dates = object@copom_dates
  )

  interp_copomscenarios(object@copom_dates, res$par)
}

di1 <- get_di1_curve(refdate) |> fixedincome::first("2 years")

cd <- get_copom_dates(di1@refdate, 6)
cm <- c(0, 0, 0, 0, 0, 0) / 1e4

interp <- fit_interpolation_with_bonds(
  interp_copomscenarios(cd, cm),
  di1, futures
)
interp@copom_moves * 1e4

interpolation(di1) <- interp
plot(di1, use_interpolation = TRUE)
