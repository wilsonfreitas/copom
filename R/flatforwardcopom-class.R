#' @include flatforwardcopom-functions.R copom-dates.R
NULL

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
    conflicts = "character",
    moves = "ANY"
  ),
  contains = "Interpolation"
)

#' FlatForwardCOPOM constructor
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
#' Creates the interpolation function to a SpotRateCurve
#' object.
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
#' @aliases
#' prepare_interpolation,COPOMScenarios,SpotRateCurve-method
#'
#' @return A `FlatForwardCOPOM` object.
#' @examples
#' library(fixedincome)
#' library(bizdays)
#' copom_dates <- as.Date(
#'   c("2022-03-17", "2022-05-05", "2022-06-17", "2022-08-04")
#' )
#' terms <- c(1, 3, 25, 44, 66, 87, 108, 131, 152, 172, 192, 214, 236, 277)
#' rates <- c(
#'   0.1065, 0.1064, 0.111, 0.1138, 0.1168, 0.1189, 0.1207, 0.1219,
#'   0.1227, 0.1235, 0.1234, 0.1236, 0.1235, 0.1235
#' )
#' curve <- spotratecurve(
#'   rates, terms, "discrete", "business/252", "Brazil/ANBIMA",
#'   refdate = as.Date("2022-02-23")
#' )
#' prepare_interpolation(
#'   interp_flatforwardcopom(copom_dates, "second"),
#'   curve
#' )
#' @export
setMethod(
  "prepare_interpolation",
  signature(object = "FlatForwardCOPOM", x = "SpotRateCurve"),
  function(object, x, ...) {
    x@interpolation <- NULL

    parts <- split_curve_into_copom_dates(x, object@copom_dates)
    results <- copom_calc(parts, 1, conflicts = object@conflicts)
    zero_curve <- do.call(c, lapply(results, function(x) x$zero))
    zero_curve <- c(x[1], zero_curve, x[x@terms > max(zero_curve@terms)])

    object@moves <- calc_moves_and_forwards(results)

    terms <- as.numeric(zero_curve@terms)
    prices <- compound(zero_curve)
    interp_coords <- xy.coords(terms, log(prices))
    interp_fun <- approxfun(interp_coords, method = "linear")
    dc <- zero_curve@daycount
    comp <- zero_curve@compounding
    object@func <- function(term_) {
      log_price <- interp_fun(term_)
      price <- exp(log_price)
      implied_rate(comp, toyears(dc, term(term_, "days")), price)
    }
    object
  }
)