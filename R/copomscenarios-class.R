#' @include flatforwardcopom-functions.R copom-dates.R
NULL

#' COPOMScenarios class
#'
#' COPOMScenarios class is an interpolation that uses scenarios for spot rates
#' after COPOM meetings together with flat forward COPOM interpolation.
#'
#' The scenarios can be defined in two ways:
#'
#' - future values for SELIC rates
#' - future values for changes in SELIC rates
#'
#' both associated to each meeting.
#'
#' @seealso FlatForwardCOPOM
#' @seealso interp_flatforwardcopom
#'
#' @export
setClass(
  "COPOMScenarios",
  slots = c(
    copom_dates = "ANY",
    future_rates = "numeric",
    copom_moves = "numeric"
  ),
  contains = "Interpolation"
)

#' COPOMScenarios constructor
#'
#' `interp_copomscenarios` creates the Interpolation object.
#'
#' @param copom_dates a vector of `Date` objects.
#' @param copom_moves a numeric vector with scenarios for changes in short term
#'        rates.
#' @param future_rates a numeric vector with scenarios for short term rates.
#'
#' `copom_dates` is a vector with de dates of the meetings that must be
#' considered in the interpolation.
#'
#' `copom_moves` is a vector with the scenarios for changes in short term rates,
#' in this case, SELIC rates.
#' The scenarios are decimal (not basis points or percentual), so a scenario of
#' 100 bps must be declared as 0.01, for example.
#' `future_rates` is a numeric vector with scenarios for future values of the
#' short term rate.
#' `copom_moves` and `future_rates` arguments are complementary,
#' if one is set, the other must be `NULL`.
#'
#' @return A `COPOMScenarios` object.
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
#'   moves <- c(50, 0, 0, 0) / 1e4
#'   interpolation(curve) <- interp_copomscenarios(copom_dates, moves)
#' }
#' @export
interp_copomscenarios <- function(copom_dates, copom_moves = numeric(0),
                                  future_rates = numeric(0)) {
  if (length(copom_moves) && length(future_rates)) {
    stop("Provide copom_moves or future_rates, not both")
  }
  obj <- new("COPOMScenarios", "copomscenarios",
    copom_dates = copom_dates,
    copom_moves = copom_moves,
    future_rates = future_rates
  )
  obj@propagate <- FALSE
  obj
}


#' @export
setMethod(
  "prepare_interpolation",
  signature(object = "COPOMScenarios", x = "SpotRateCurve"),
  function(object, x, ...) {
    x@interpolation <- NULL

    t1 <- term(bizdays(x@refdate, object@copom_dates, x@calendar), "days")
    t1 <- c(t1, x@terms[x@terms > max(t1)][1])
    t <- t1 - as.numeric(shift(t1, fill = 0))

    if (length(object@copom_moves)) {
      acc_moves <- c(0, cumsum(object@copom_moves))
      object@future_rates <- as.numeric(as.spotrate(x[1]) + acc_moves)[-1]
    }
    fwd <- c(as.spotrate(x[1]), object@future_rates)
    if (!length(object@copom_moves)) {
      object@copom_moves <- as.numeric(diff(fwd))
    }
    comp <- cumprod(compound(fwd, t))

    ix <- x@terms > max(t1)
    terms <- c(term(1, "days"), t1, x@terms[ix])
    if (any(ix)) {
      prices <- c(compound(x[1]), comp, compound(x[ix]))
    } else {
      prices <- c(compound(x[1]), comp)
    }
    interp_coords <- xy.coords(terms, log(prices))
    interp_fun <- approxfun(interp_coords, method = "linear")
    dc <- x@daycount
    comp <- x@compounding
    object@func <- function(term_) {
      log_price <- interp_fun(term_)
      price <- exp(log_price)
      implied_rate(comp, toyears(dc, term(term_, "days")), price)
    }
    object
  }
)

#' Fit COPOM moves according to a SpotRateCurve
#'
#' Finds the COPOM moves (or future rates) that yields the least interpolation
#' error.
#'
#' @param object a COPOMScenarios object with initial parameters set.
#' @param x a SpotRateCurve object.
#' @param ... additional arguments. Currently unused.
#'
#' @return A `Interpolation` object.
#' @examples
#' \dontrun{
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
#' cd <- get_copom_dates(curve@refdate, 4)
#' cm <- c(50, 25, 0, 0) / 1e4
#' interpolation(curve) <- fit_interpolation(
#'   interp_copomscenarios(cd, cm),
#'   curve
#' )
#' }
#' @export
setMethod(
  "fit_interpolation",
  signature(object = "COPOMScenarios", x = "SpotRateCurve"),
  function(object, x, ...) {
    x <- filter_curve_to_optim(x, object@copom_dates)
    par <- object@copom_moves
    res <- optim(par, function(par, x, .dates) {
      interpolation(x) <- interp_copomscenarios(.dates, par)
      interpolation_error(x)
    }, method = "BFGS", x = x, .dates = object@copom_dates)
    interp_copomscenarios(object@copom_dates, res$par)
  }
)

filter_curve_to_optim <- function(curve, copom_dates) {
  curve@interpolation <- NULL
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