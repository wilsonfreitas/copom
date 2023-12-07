library(dplyr)
library(stringr)
source("examples/utils-functions.R")
devtools::load_all()

library(rb3)
library(bizdays)
library(fixedincome)
library(copom)
library(tidyverse)

# refdate <- getdate("last bizday", Sys.Date(), "Brazil/ANBIMA")
refdate <- as.Date("2022-09-05")
fut <- futures_get(refdate)
yc <- yc_get(refdate)
df <- yc_superset(yc, fut)

df_curve <- bind_rows(
  df |> filter(biz_days == 1) |> select(biz_days, r_252),
  df |> filter(!is.na(symbol)) |> select(biz_days, r_252)
) |>
  filter(!duplicated(biz_days))

di1 <- spotratecurve(
  df_curve$r_252, df_curve$biz_days, "discrete", "business/252", "Brazil/ANBIMA",
  refdate = refdate
)

di1_1y <- di1 |> fixedincome::first("1 years")
cd <- get_copom_dates(di1@refdate, 6)
cm <- c(0, 0, 0, 0, -25, -25) / 1e4
interpolation(di1_1y) <- interp_copomscenarios(cd, cm)
autoplot(di1_1y, curve.geom = "point") +
  autolayer(di1_1y,
    curve.interpolation = TRUE, curve.geom = "line",
    curve.name = "Interpolation"
  )

cm <- c(0, 0, 0, 0, 0, 0) / 1e4
interpolation(di1_1y) <- interp_copomscenarios(cd, cm)
autoplot(di1_1y, curve.geom = "point") +
  autolayer(di1_1y,
    curve.interpolation = TRUE, curve.geom = "line",
    curve.name = "Interpolation"
  )

# FOCUS scenarios

df <- rbcb::get_market_expectations("top5s-selic", start_date = "2022-04-29")

df <- df |>
  mutate(
    Reuniao_ano = str_split(reuniao, "/", simplify = TRUE)[, 2],
    Reuniao_cod = str_split(reuniao, "/", simplify = TRUE)[, 1],
    reuniao = str_c(Reuniao_ano, Reuniao_cod)
  ) |>
  arrange(reuniao) |>
  filter(tipoCalculo == "C")

fwd <- (df[["mediana"]][2:8] - 0.1) / 100
interpolation(di1) <- interp_copomscenarios(cd, forward_rates = fwd)
plot(di1 |> fixedincome::first("1 years"), use_interpolation = TRUE)

# ----

# optimize COPOM moves against a given curve
interp <- fit_interpolation(interp_copomscenarios(cd, cm), di1)
interp@copom_moves * 1e4
interpolation(di1) <- interp
plot(di1, use_interpolation = TRUE)

# optimize COPOM moves against bonds

fit_interpolation_with_bonds <- function(object, x, bonds) {
  par <- object@copom_moves
  res <- optim(par, function(par, x, .dates) {
    interpolation(x) <- interp_copomscenarios(.dates, par)
  }, method = "BFGS", x = x, .dates = object@copom_dates)
  interp_copomscenarios(object@copom_dates, res$par)
}

df <- rbcb::get_market_expectations("top5s-selic", start_date = "2022-04-29")

df |>
  mutate(
    Reuniao_ano = str_split(reuniao, "/", simplify = TRUE)[, 2],
    Reuniao_cod = str_split(reuniao, "/", simplify = TRUE)[, 1],
    reuniao = str_c(Reuniao_ano, Reuniao_cod)
  ) |>
  arrange(reuniao) |>
  filter(tipoCalculo == "C")

str_split(df$Reuniao, "/", simplify = TRUE)

# di1[[1:100]]
# fixedincome::first(di1, "2 years")
# library(stringr)
# library(rbcb)
# library(purrr)
# selic <- get_series(c(SELIC = 432), start_date = "2022-04-01")
# selic_exp <- get_market_expectations("selic", start_date = "2022-03-25") |>
#     filter(baseCalculo == 0) |>
#     mutate(Reuniao = str_split(Reuniao, "/") |> map_chr(~ paste0(.x[2], .x[1]))) |>
#     arrange(Reuniao)
# cd <- copom_dates[copom_dates > "2022-04-01"]
# mv <- c(selic_exp$Mediana[1] - selic$SELIC[1], selic_exp$Mediana |> diff())
# mv[seq_along(cd)]
# copom_dates |> diff()
# copom_dates |> bizdiff("Brazil/ANBIMA")
# adjust for the first future
# cd <- copom_dates[copom_dates > di1@refdate][1:6]
# cm <- c(100, 100, 50, 50, 0, 0) / 1e4
# t1 <- term(bizdays(di1@refdate, cd, di1@calendar), "days")
# t1 <- c(t1, di1@terms[di1@terms > max(t1)][1])
# t <- t1 - shift(t1, fill = 0)
# acc_moves <- c(0, cumsum(cm))
# comp <- cumprod(compound(as.spotrate(di1[1]) + acc_moves, t))
# ix <- di1@terms > max(t1)
# terms <- c(term(1, "days"), t1, di1@terms[ix])
# prices <- c(compound(di1[1]), comp, compound(di1[ix]))
# plot(terms, prices, type = "b")
