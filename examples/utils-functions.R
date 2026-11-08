
library(glue)
library(stringr)
library(ggplot2)
library(dplyr)
library(bizdays)
library(forcats)
library(rbcb)
library(rb3)
library(fixedincome)
devtools::load_all() # run the scripts from the package root

# DI1 curve from B3 data: the 1-day vertex plus the vertices of the DI1 futures.
# Written for rb3 0.1.0. To be replaced by an rb3 helper:
# https://github.com/ropensci/rb3/issues/130
get_di1_curve <- function(refdate) {
  refdate <- as.Date(refdate)
  fetch_marketdata("b3-reference-rates", refdate = refdate, curve_name = "PRE")
  fetch_marketdata("b3-futures-settlement-prices", refdate = refdate)
  # rb3 returns an extra row with NA rates for each vertex
  df <- yc_brl_with_futures_get(refdate) |> filter(!is.na(r_252))

  df_curve <- bind_rows(
    df |> filter(biz_days == 1) |> select(biz_days, r_252),
    df |> filter(!is.na(symbol)) |> select(biz_days, r_252)
  ) |>
    filter(!duplicated(biz_days))

  spotratecurve(
    df_curve$r_252, df_curve$biz_days, "discrete", "business/252", "Brazil/ANBIMA",
    refdate = refdate
  )
}

plot_curve <- function(curve, copom_dates, base_size = 20) {
  curve_fwd <- forwardrate(curve)
  curve_fwd <- spotratecurve(
    as.numeric(curve_fwd),
    cumsum(curve_fwd@terms),
    refdate = curve@refdate,
    .copyfrom = curve
  ) |> as.data.frame()
  curve_spt <- as.data.frame(curve)

  .dash <- "#4f7f81"
  .colors <- c("#e05305", "#fbb407")
  .names <- c("Curva Zero", "Curva Forward")
  names(.colors) <- .names

  g <- ggplot() +
    geom_vline(
      xintercept = copom_dates, colour = .dash,
      linetype = "dashed", size = 1
    ) +
    geom_line(
      data = curve_spt,
      mapping = aes(x = dates, y = rates, colour = .names[1]),
      size = 1
    ) +
    geom_point(
      data = curve_spt,
      mapping = aes(x = dates, y = rates, colour = .names[1]),
      size = 2
    ) +
    geom_step(
      data = curve_fwd,
      mapping = aes(x = dates, y = rates, colour = .names[2]),
      size = 1,
      direction = "vh"
    ) +
    geom_point(
      data = curve_fwd,
      mapping = aes(x = dates, y = rates, colour = .names[2]),
      size = 2
    )

  g <- g +
    scale_colour_manual("", breaks = .names, values = .colors)

  .title <- glue("Curva de Juros Prefixados DI1 - {refdate}",
    refdate = format(curve@refdate)
  )
  g <- g +
    labs(
      x = "Data",
      y = "%",
      title = .title,
      subtitle = "As linhas cinza tracejadas representam as datas do COPOM",
      caption = "Desenvolvido por wilsonfreitas (com dados da B3)"
    ) +
    theme_wf(base_size = base_size)
  g
}

plot_copom_curve <- function(curve, copom_curve, copom_dates, base_size = 16) {
  curve_spt <- as.data.frame(curve)
  curve_fwd <- forwardrate(curve)
  curve_fwd <- spotratecurve(
    as.numeric(curve_fwd),
    cumsum(curve_fwd@terms),
    refdate = curve@refdate,
    .copyfrom = curve
  ) |> as.data.frame()

  .dash <- "#4f7f81"
  .colors <- c("#e05305", "#fbb407")
  .names <- c("COPOM Forward", "DI1 Forward")
  names(.colors) <- .names

  g <- ggplot() +
    geom_vline(
      xintercept = copom_dates,
      colour = "grey",
      linetype = "dashed", size = 1
    ) +
    geom_step(
      data = curve_fwd,
      mapping = aes(x = dates, y = rates, colour = .names[2]),
      size = 1,
      direction = "vh"
    ) +
    geom_point(
      data = curve_fwd,
      mapping = aes(x = dates, y = rates, colour = .names[2]),
      size = 2
    ) +
    geom_step(
      data = copom_curve,
      mapping = aes(x = dates, y = rates_forward, colour = .names[1]),
      size = 1,
      direction = "hv"
    ) +
    geom_point(
      data = copom_curve,
      mapping = aes(x = dates, y = rates_forward, colour = .names[1]),
      size = 2
    )

  g <- g +
    scale_colour_manual("", breaks = .names, values = .colors)

  .title <- glue("Curva a Termo de Juros Prefixados DI1 - {refdate}",
    refdate = format(curve@refdate)
  )
  g <- g +
    labs(
      x = "Data",
      y = "%",
      title = .title,
      subtitle = "As linhas cinza tracejadas representam as datas do COPOM",
      caption = "Desenvolvido por wilsonfreitas (com dados da B3)"
    ) +
    theme_wf(base_size = base_size)

  g
}

# Paleta de cores
# https://icolorpalette.com/e05305_4f7f81_fbb407_5d91a2_432608
theme_wf <- function(base_size = 12,
                     base_family = "mono",
                     base_line_size = base_size / 22,
                     base_rect_size = base_size / 22) {
  theme_grey(
    base_size = base_size,
    base_family = base_family,
    base_line_size = base_line_size,
    base_rect_size = base_rect_size
  ) %+replace%
    theme(
      legend.position = "top",
      text = element_text(family = base_family, size = base_size),
      plot.background = element_blank(),
      panel.background = element_blank(),
      strip.background = element_blank(),
      panel.grid = element_line(colour = "grey92"),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      title = element_text(
        family = base_family,
        colour = "black",
        face = "bold"
      ),
      axis.line = element_line(colour = "grey92", size = 1),
      axis.title.y = element_text(colour = "black", face = "bold"),
      axis.title.x = element_text(colour = "black", face = "bold"),
      legend.key = element_rect(fill = "white", colour = NA),
      complete = TRUE
    )
}
