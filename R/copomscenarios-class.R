
setClass(
  "COPOMScenarios",
  slots = c(
    copom_dates = "ANY",
    forward_rates = "numeric",
    copom_moves = "numeric"
  ),
  contains = "Interpolation"
)

interp_copomscenarios <- function(copom_dates, copom_moves = numeric(0),
                                  forward_rates = numeric(0)) {
  if (length(copom_moves) && length(forward_rates)) {
    stop("Provide copom_moves or forward_rates, not both")
  }
  obj <- new("COPOMScenarios", "copomscenarios",
    copom_dates = copom_dates,
    copom_moves = copom_moves,
    forward_rates = forward_rates
  )
  obj@propagate <- FALSE
  obj
}

setMethod(
  "prepare_interpolation",
  signature(object = "COPOMScenarios", x = "SpotRateCurve"),
  function(object, x, ...) {
    x@interpolation <- NULL

    t1 <- term(bizdays(x@refdate, object@copom_dates, x@calendar), "days")
    t1 <- c(t1, x@terms[x@terms > max(t1)][1])
    t <- t1 - shift(t1, fill = 0)

    if (length(object@copom_moves)) {
      acc_moves <- c(0, cumsum(object@copom_moves))
      object@forward_rates <- as.numeric(as.spotrate(x[1]) + acc_moves)[-1]
    }
    fwd <- c(as.spotrate(x[1]), object@forward_rates)
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
    object@func <- function(term) {
      log_price <- interp_fun(term)
      price <- exp(log_price)
      rates(comp, toyears(dc, term, "days"), price)
    }
    object
  }
)

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