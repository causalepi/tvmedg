#' Helper to adjust styling of a plot
#'
#' @param ... arbitrary params
#'
#' @import ggplot2
#' @return
#' list of updated aesthetic values
#' @export
mytheme <- function(...) {
  theme_minimal() +
    theme(
      plot.title = element_text(size = 14,color = "grey10",  face = "bold", hjust = 0.5),
      axis.line = element_line(linetype = "solid"),
      axis.text = element_text(color = "gray10", size = 10),
      axis.title = element_text(color = "gray10", size = 12),
      # plot.background = element_blank(),
      panel.background = element_rect(fill = "white", color = NA),
      legend.title = element_text(size = 12, face = "bold"),
      legend.direction = "horizontal",
      legend.position = "top",
      legend.background = element_rect(fill = NA, color = NA),
      legend.text = element_text(size = 12),
      legend.key.width = unit(1, "line"),
      strip.text = element_text(size = 12, face = "bold"),
      strip.background = element_rect(fill = NA, color = NA)
    )
}


#' Plot function
#'
#' @param x data
#' @param what Cummulative Y or time-varying Y or both
#' @param ... arbitrary params
#'
#' @import dplyr
#' @import ggplot2
#' @importFrom cowplot plot_grid
#'
#'
#' @return
#' plot
#' @export
plot.tvmedg <- function(x,what = c("all","cumY","tvY"),...){

  tv_cumsum <- x$ori_df |>
    group_by(j) |>
    summarise(
      y_prop = mean(Y),
      y_count = sum(Y)
    ) |>
    ungroup() |>
    mutate(y_prob_cum = cumsum(y_count)/nrow(x$ori_df))

  dat1M <- x$dat_MC |>
    mutate(group = if_else(Ay == 1 & Am == 1, "Q(1,1)",
                           if_else(Ay == 1 & Am ==0, "Q(1,0)", "Q(0,0)")),
           group = factor(group, labels = c("Q(0,0)", "Q(1,0)", "Q(1,1)")),
           groupM = factor(Am, labels = c("No", "Yes"))) |>
    group_by(group, mm) |>
    summarise(Y = mean(Yp2)) |>
    ungroup() |>
    group_by(group) |>
    mutate(Ysum = cumsum(Y))|>
    ungroup()

  ## cumY

  cumy_dfplot <- dat1M |>
    left_join(tv_cumsum, by = join_by(mm == j))

  y_limt <- max(cumy_dfplot$Ysum,cumy_dfplot$y_prob_cum,na.rm = T)

  f_cumY <- cumy_dfplot |>
    ggplot() +
    geom_line(aes(x = mm, y = Ysum, color = group), linewidth = 1) +
    geom_line(aes(x = mm, y = y_prob_cum), color = "gray20",
              linewidth = 1, linetype = 2) +
    scale_color_brewer(palette = "Set1", direction = -1) +
    scale_y_continuous(limits = c(0, round(y_limt+0.4,1))) +
    scale_x_continuous(breaks = seq(0, max(dat1M$mm), by = 12)) +
    mytheme() +
    labs(x = "Month",
         y = "%",
         title = "Cumulative Y (%)",
         caption = "The dashed line represents the observed %",
         color = NULL)

  ## tvY
  tvy_dfplot <- dat1M |>
    left_join(tv_cumsum, by = join_by(mm == j))

  y_limt2 <- max(tvy_dfplot$Y,tvy_dfplot$y_prop,na.rm=TRUE)

  f_tvY <- tvy_dfplot |>
    ggplot() +
    geom_line(aes(x = mm, y = Y, color = group), linewidth = 1) +
    geom_line(aes(x = mm, y = y_prop), color = "gray20",
              linewidth = 1, linetype = 2) +
    scale_color_brewer(palette = "Set1", direction = -1) +
    scale_y_continuous(limits = c(0, round(y_limt2+0.02,1))) +
    scale_x_continuous(breaks = seq(0, max(dat1M$mm), by = 12)) +
    mytheme() +
    labs(x = "Month",
         y = "%",
         title = "Y by month (%)",
         caption = "The dashed line represents the observed %",
         color = NULL)

  if (what == c("all")){
    out_plot <- plot_grid(f_cumY,f_tvY,nrow = 1)
  } else if (what == "cumY") {
    out_plot <- f_cumY
  } else {
    out_plot <- f_tvY
  }

  out_plot

}


