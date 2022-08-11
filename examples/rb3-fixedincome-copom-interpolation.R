
library(rb3)
library(bizdays)
library(fixedincome)
library(copom)
library(tidyverse)

# refdate <- getdate("last bizday", Sys.Date(), "Brazil/ANBIMA")
refdate <- as.Date("2022-03-09")
fut <- futures_get(refdate)
yc <- yc_get(refdate)
df <- yc_superset(yc, fut)

df_curve <- bind_rows(
  df |> slice(1) |> select(biz_days, r_252),
  df |> filter(!is.na(symbol)) |> select(biz_days, r_252)
) |>
  filter(!duplicated(biz_days))

di1_curve <- spotratecurve(
  df_curve$r_252, df_curve$biz_days, "discrete", "business/252", "Brazil/ANBIMA",
  refdate = refdate
)


di1_curve_part <- fixedincome::first(di1_curve, "3 years")
copom_dates <- get_copom_dates(refdate, 8)
interpolation(di1_curve_part) <- interp_flatforwardcopom(copom_dates, "optimize")
interpolation(di1_curve_part)@moves$moves * 1e4

plot(di1_curve_part, show_forward = TRUE)
