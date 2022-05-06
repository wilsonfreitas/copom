
source("examples/utils-functions.R")
source("examples/copomscenarios.R")

refdate <- "2022-04-04"
di1 <- get_curve_from_web(refdate) |> fixedincome::first("2 years")
copom_dates <- get_copom_dates(di1@refdate, 6)

filter_curve_to_optim <- function(curve, copom_dates) {
  splits <- split_curve_into_copom_dates(curve, copom_dates)

  fs <- lapply(splits, function(x) {
    if (length(x$futures) > 1) {
      x$futures[2]
    } else {
      x$futures
    }
  })

  c(curve[1], do.call(c, fs))
}

filter_curve_to_optim(di1, copom_dates)