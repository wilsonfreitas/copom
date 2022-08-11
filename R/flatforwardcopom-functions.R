split_curve_into_copom_dates <- function(curve, copom_dates) {
  curve_fwd <- forwardrate(curve)
  dates_terms <- maturities(curve)
  lapply(seq_along(copom_dates), function(x) {
    gte <- dates_terms > copom_dates[x]
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
    fwd_copom@terms <- du_copom - as.numeric(last_result$zero@terms)
    fwd_rates <- forwardrate(last_result$zero)
    # compose the zero with the rates up to the last copom date and
    # from the last to the next copom date
    fwd_rates <- c(fwd_rates, fwd_copom)
    spot_curve <- as.spotratecurve(fwd_rates, last_result$zero@refdate)
    spot_curve[[du_copom]]
  }
}

calc_moves_and_forwards <- function(results) {
  fwd <- do.call(c, lapply(results, function(x) x[["copom_forward"]]))
  zero <- do.call(c, lapply(results, function(x) x[["zero"]]))
  moves <- diff(fwd) |> as.numeric()
  moves <- c((fwd[1] - zero[1]) |> as.numeric(), moves)
  dates <- do.call(c, lapply(results, function(x) x[["copom_date"]]))
  data.frame(
    dates,
    forward_rates = fwd,
    moves = as.numeric(moves)
  )
}

copom_calc <- function(parts, x = 1, results = NULL,
                       conflicts = c(
                         "forward", "second", "first", "optimize"
                       )) {
  if (x > length(parts)) {
    return(results)
  }

  conflicts <- match.arg(conflicts)
  futs <- parts[[x]]$futures
  result <- if (length(futs) >= 2) {
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
  forwardrate(futs, as.numeric(du_copom), futs@terms[idx + 1])
}

forward_calc_use_second_future <- function(futs, fwds, du_copom, zero) {
  futs <- futs[-1]
  futs[[du_copom]] <- zero
  idx <- match(du_copom, futs@terms)
  forwardrate(futs, as.numeric(du_copom), futs@terms[idx + 1])
}

forward_calc_use_forward <- function(futs, fwds, du_copom, zero) {
  fwds[2]
}

forward_calc_optim <- function(futs, fwds, du_copom, zero) {
  du_fwd <- as.numeric(futs@terms) - as.numeric(du_copom)
  du_fwd <- term(du_fwd)
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
