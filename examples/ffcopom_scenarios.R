source("examples/utils-functions.R")
library(tidyverse)

# refdate <- getdate("last bizday", Sys.Date(), "Brazil/ANBIMA")
refdate <- as.Date("2022-09-05")
di1 <- get_di1_curve(refdate)

di1_1y <- di1 |> fixedincome::first("2 years")
cd <- get_copom_dates(di1@refdate, 6)
cm <- c(0, 0, 0, 0, -25, -25) / 1e4
interpolation(di1_1y) <- interp_copomscenarios(cd, cm)
autoplot(di1_1y, curve.geom = "point") +
  autolayer(di1_1y,
    curve.interpolation = TRUE, curve.geom = "line",
    curve.name = "Interpolation"
  )

cm <- c(0, 0, -25, 0, 0, 0) / 1e4
interpolation(di1_1y) <- interp_copomscenarios(cd, cm)
autoplot(di1_1y, curve.geom = "point") +
  autolayer(di1_1y,
    curve.interpolation = TRUE, curve.geom = "line",
    curve.name = "Interpolation"
  )

# FOCUS scenarios

# last survey published up to the curve's reference date
df <- rbcb::get_market_expectations("top5s-selic",
  start_date = refdate - 7, end_date = refdate
)
df <- df |> filter(Data == max(Data))

df <- df |>
  mutate(
    Reuniao_ano = str_split(reuniao, "/", simplify = TRUE)[, 2],
    Reuniao_cod = str_split(reuniao, "/", simplify = TRUE)[, 1],
    reuniao = str_c(Reuniao_ano, Reuniao_cod)
  ) |>
  arrange(reuniao) |>
  filter(tipoCalculo == "C")

# one expectation per meeting in `cd`; minus 0.10 pp to go from SELIC to CDI
fwd <- (df[["mediana"]][seq_along(cd)] - 0.1) / 100
interpolation(di1) <- interp_copomscenarios(cd, future_rates = fwd)
plot(di1 |> fixedincome::first("1 years"), use_interpolation = TRUE)

# ----

# optimize COPOM moves against a given curve
interp <- fit_interpolation(interp_copomscenarios(cd, cm), di1)
interp@copom_moves * 1e4
interpolation(di1) <- interp
plot(di1, use_interpolation = TRUE)
