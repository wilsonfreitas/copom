#' FlatForwardCOPOM interpolation
#'
#' FlatForwardCOPOM interpolation extends Interpolation class from `fixedincome`
#' package.
#'
#' This interpolation considers that the interest rate between COPOM meetings
#' in Brazil are flat, instead of being flat betweet bonds maturities.
#' This is mainly relevant for the short term of the term structure where
#' the interpolation is used to price private bonds and interest rate
#' derivatives.
#' This is discussed in the book Brazilian Derivatives and Securities.
#'
#' @references Marcos C. S. Carreira and Richard J. Brostowicz.
#'             Brazilian Derivatives and Securities, Palgrave Macmillan, 2016
#' @export
setClass(
  "FlatForwardCOPOM",
  slots = c(
    copom_dates = "ANY",
    conflicts = "character"
  ),
  contains = "Interpolation"
)

#' Interpolation constructor
#'
#' `interp_flatforwardcopom` creates the Interpolation object.
#'
#' @param copom_dates a vector of `Date` objects.
#' @param conflicts a character with one of: "forward", "second", "first",
#'                  "optimize"
#'
#' `copom_dates` is a vector with de dates of the meetings that must be
#' considered in the interpolation.
#'
#' `conflicts` specify how to deal with the conflicts that appear in the
#' interpolation.
#' These conflicts appear when two futures exist between two meeting.
#' For one future between two meeting the decision is done, otherwise,
#' the following alternatives are considered:
#'
#' \itemize{
#'    \item `forward` used the forward rate between these two futures.
#'    \item `first` use the first future only.
#'    \item `second` use the second future only (this is suggested by
#'          Carreira and Brostowicz).
#'    \item `optimize` runs an optimization with the two futures.
#' }
#'
#' @return A `FlatForwardCOPOM` object.
#' @examples
#' if (require(fixedincome) && require(bizdays)) {
#'   copom_dates <- as.Date(
#'     c("2022-03-17", "2022-05-05", "2022-06-17", "2022-08-04")
#'   )
#'   terms <- c(1, 3, 25, 44, 66, 87, 108, 131, 152, 172, 192, 214, 236, 277)
#'   rates <- c(
#'     0.1065, 0.1064, 0.111, 0.1138, 0.1168, 0.1189, 0.1207, 0.1219,
#'     0.1227, 0.1235, 0.1234, 0.1236, 0.1235, 0.1235
#'   )
#'   curve <- spotratecurve(
#'     rates, terms, "discrete", "business/252", "Brazil/ANBIMA",
#'     refdate = as.Date("2022-02-23")
#'   )
#'   interpolation(curve) <- interp_flatforwardcopom(copom_dates, "second")
#' }
#' @export
interp_flatforwardcopom <- function(copom_dates, conflicts) {
  new("FlatForwardCOPOM", "flatforwardcopom",
    copom_dates = copom_dates,
    conflicts = conflicts
  )
}

#' Create the interpolation function
#'
#' Creates the interpolation function to a SpotRateCurve object.
#'
#' @param object a Interpolation object.
#' @param x a SpotRateCurve object.
#' @param ... additional arguments. Currently unused.
#'
#' This method is used internally when the interpolation is set to a curve.
#' It uses the current state of the curve to build the interpolation function.
#' This is similar to call `approxfun` and `splinefun` to create functions that
#' perform interpolation of the given data points.
#'
#' This method shouldn't be directly called, it is for internal use only.
#'
#' @return A `FlatForwardCOPOM` object.
#' @examples
#' if (require(fixedincome) && require(bizdays)) {
#'   copom_dates <- as.Date(
#'     c("2022-03-17", "2022-05-05", "2022-06-17", "2022-08-04")
#'   )
#'   terms <- c(1, 3, 25, 44, 66, 87, 108, 131, 152, 172, 192, 214, 236, 277)
#'   rates <- c(
#'     0.1065, 0.1064, 0.111, 0.1138, 0.1168, 0.1189, 0.1207, 0.1219,
#'     0.1227, 0.1235, 0.1234, 0.1236, 0.1235, 0.1235
#'   )
#'   curve <- spotratecurve(
#'     rates, terms, "discrete", "business/252", "Brazil/ANBIMA",
#'     refdate = as.Date("2022-02-23")
#'   )
#'   prepare_interpolation(
#'     interp_flatforwardcopom(copom_dates, "second"),
#'     curve
#'   )
#' }
#' @export
setMethod(
  "prepare_interpolation",
  signature(object = "FlatForwardCOPOM", x = "SpotRateCurve"),
  function(object, x, ...) {
    x@interpolation <- NULL

    parts <- split_curve_into_copom_dates(x, object@copom_dates)
    zero_curve <- copom_calc(parts, 1, conflicts = object@conflicts)
    zero_curve <- c(x[1], zero_curve, x[x@terms > max(zero_curve@terms)])

    terms <- as.numeric(zero_curve@terms)
    prices <- compound(zero_curve)
    interp_coords <- xy.coords(terms, log(prices))
    interp_fun <- approxfun(interp_coords, method = "linear")
    dc <- zero_curve@daycount
    comp <- zero_curve@compounding
    object@func <- function(term) {
      log_price <- interp_fun(term)
      price <- exp(log_price)
      rates(comp, toyears(dc, term, "days"), price)
    }
    object
  }
)

#' Historical COPOM dates
#'
#' Historical COPOM dates since 2015.
#'
#' @param date reference date
#' @param n number of dates returned
#'
#' If `date` is `NULL` all dates are returned.
#' Once `date` is set, `n` dates greater than or equal to `date` are returned.
#'
#' @return A vector with `Date` objects.
#' @examples
#' get_copom_dates()
#' get_copom_dates("2022-01-04", 4)
#' @export
get_copom_dates <- function(date = NULL, n = 8) {
  copom_dates <- c(
    "2015-01-21", "2015-03-04", "2015-04-29", "2015-06-03", "2015-07-29",
    "2015-09-02", "2015-10-21", "2015-11-25",
    "2016-01-20", "2016-03-02", "2016-04-27", "2016-06-08", "2016-07-20",
    "2016-08-31", "2016-10-19", "2016-11-29",
    "2017-01-11", "2017-02-22", "2017-04-12", "2017-05-31", "2017-07-26",
    "2017-09-06", "2017-10-25", "2017-12-06",
    "2018-02-07", "2018-03-21", "2018-05-16", "2018-06-20", "2018-08-01",
    "2018-09-19", "2018-10-31", "2018-12-12",
    "2019-02-06", "2019-03-20", "2019-05-08", "2019-06-19", "2019-07-31",
    "2019-09-18", "2019-10-30", "2019-12-11",
    "2020-02-05", "2020-03-18", "2020-05-06", "2020-06-17", "2020-08-05",
    "2020-09-16", "2020-10-28", "2020-12-09",
    "2021-01-20", "2021-03-17", "2021-05-05", "2021-06-16", "2021-08-04",
    "2021-09-22", "2021-10-27", "2021-12-08",
    "2022-02-02", "2022-03-16", "2022-05-04", "2022-06-15", "2022-08-03",
    "2022-09-21", "2022-10-26", "2022-12-07",
    "2023-01-18", "2023-02-28"
  )

  x <- sort(as.Date(copom_dates))
  if (is.null(date)) {
    x
  } else {
    date <- as.Date(date)
    ix <- x >= date
    dates <- copom_dates[ix]
    dates[seq_len(n)]
  }
}

split_curve_into_copom_dates <- function(curve, copom_dates) {
  curve_fwd <- forwardrate(curve)
  dates_terms <- maturities(curve)
  lapply(seq_along(copom_dates), function(x) {
    gte <- dates_terms >= copom_dates[x]
    lte <- dates_terms <= copom_dates[x + 1]
    if (x == 1) {
      idx <- which(gte & lte)
      seed_rate <- as.numeric(curve[1])
    } else if (!is.na(copom_dates[x + 1])) {
      idx <- which(gte & lte)
      seed_rate <- NA_real_
    } else {
      idx <- which(gte)
      seed_rate <- NA_real_
    }
    list(
      copom_date = copom_dates[x],
      futures = curve[idx],
      forward = curve_fwd[idx],
      seed_rate = seed_rate
    )
  })
}

calc_zero <- function(last_result, du_copom, futs, seed_rate) {
  if (is.null(last_result)) {
    spotratecurve(
      seed_rate,
      du_copom,
      refdate = futs@refdate,
      .copyfrom = futs
    )
  } else {
    # this is the forward rate that starts at the last copom date
    fwd_copom <- last_result$copom_forward
    # replace the terms to extend it up to the next copom date
    fwd_copom@terms <- du_copom - last_result$zero@terms
    fwd_rates <- forwardrate(last_result$zero)
    # compose the zero with the rates up to the last copom date and
    # from the last to the next copom date
    fwd_rates <- c(fwd_rates, fwd_copom)
    spot_curve <- as.spotratecurve(fwd_rates, last_result$zero@refdate)
    spot_curve[[du_copom]]
  }
}

copom_calc <- function(parts, x = 1, results = NULL,
                       conflicts = c(
                         "forward", "second", "first", "optimize"
                       )) {
  if (x > length(parts)) {
    zero_curve <- do.call(c, lapply(results, function(x) x$zero))
    return(zero_curve)
  }

  conflicts <- match.arg(conflicts)
  futs <- parts[[x]]$futures
  result <- if (length(futs) == 2) {
    if (conflicts == "forward") {
      calc_with(parts, x, results, forward_calc_use_forward)
    } else if (conflicts == "first") {
      calc_with(parts, x, results, forward_calc_use_first_future)
    } else if (conflicts == "second") {
      calc_with(parts, x, results, forward_calc_use_second_future)
    } else if (conflicts == "optimize") {
      calc_with(parts, x, results, forward_calc_optim)
    }
  } else if (length(futs) == 0) {
    NULL
  } else {
    calc_with(parts, x, results, forward_calc_use_first_future)
  }
  results[[length(results) + 1]] <- result
  copom_calc(parts, x + 1, results, conflicts)
}

calc_with <- function(parts, x, results, forward_calc) {
  copom_date <- parts[[x]]$copom_date
  futs <- parts[[x]]$futures
  fwds <- parts[[x]]$forward
  refdate <- futs@refdate
  du_copom <- term(bizdays(refdate, copom_date, "Brazil/ANBIMA"))

  last_result <- results[[length(results)]]

  zero <- calc_zero(last_result, du_copom, futs, parts[[x]]$seed_rate)
  fwd <- forward_calc(futs, fwds, du_copom, zero)

  list(
    copom_date = copom_date,
    zero = zero,
    copom_forward = fwd
  )
}

forward_calc_use_first_future <- function(futs, fwds, du_copom, zero) {
  futs[[du_copom]] <- zero
  idx <- match(du_copom, futs@terms)
  forwardrate(futs, du_copom, futs@terms[idx + 1])
}

forward_calc_use_second_future <- function(futs, fwds, du_copom, zero) {
  futs <- futs[-1]
  futs[[du_copom]] <- zero
  idx <- match(du_copom, futs@terms)
  forwardrate(futs, du_copom, futs@terms[idx + 1])
}

forward_calc_use_forward <- function(futs, fwds, du_copom, zero) {
  fwds[2]
}

forward_calc_optim <- function(futs, fwds, du_copom, zero) {
  du_fwd <- futs@terms - du_copom
  f_obj <- function(x) {
    spot_rate <- spotratecurve(rep(x, length(du_fwd)), du_fwd,
      refdate = zero@refdate,
      .copyfrom = futs
    )
    fact_obj <- compound(zero) * compound(spot_rate)
    sum(compound(futs) - fact_obj)^2
  }
  res <- optim(as.numeric(fwds[2]), f_obj,
    method = "Brent",
    lower = 0, upper = 1
  )
  forwardrate(res$par, du_fwd, .copyfrom = futs)
}