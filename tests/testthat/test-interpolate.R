copom_dates <- as.Date(c("2022-03-17", "2022-05-05", "2022-06-17", "2022-08-04"))
terms <- c(1, 3, 25, 44, 66, 87, 108, 131, 152, 172, 192, 214, 236, 277)
rates <- c(
  0.1065, 0.1064, 0.111, 0.1138, 0.1168, 0.1189, 0.1207, 0.1219,
  0.1227, 0.1235, 0.1234, 0.1236, 0.1235, 0.1235
)
curve <- spotratecurve(
  rates, terms, "discrete", "business/252", "Brazil/ANBIMA",
  refdate = as.Date("2022-02-23")
)

test_that("interpolate returns numeric (issue #1)", {
  interps <- list(
    interp_flatforwardcopom(copom_dates, "second"),
    interp_copomscenarios(copom_dates, c(50, 0, 0, 0) / 1e4)
  )
  for (interp in interps) {
    interpolation(curve) <- interp
    expect_identical(class(interpolate(curve@interpolation, c(10, 50))), "numeric")
    expect_identical(class(interpolate(curve@interpolation, term(c(10, 50), "days"))), "numeric")
  }
})
