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
#'
#'
#' @return
#' plot
#' @export
plot.tvmedg <- function(x, what = c("cumY","tvY"),...){

  tv_cumsum <- x$ori_df |>
    group_by(j) |>
    summarise(
      y_prop = mean(Y),
      y_count = sum(Y)
    ) |>
    ungroup() |>
    mutate(y_prob_cum = cumsum(y_count)/length(unique(x$ori_df$id)))

  dat1Ma <- x$dat_MC |>
    mutate(group = if_else(Ay == 1 & Am == 1, "Q(1,1)",
                           if_else(Ay == 1 & Am ==0, "Q(1,0)", "Q(0,0)")),
           group = factor(group, labels = c("Q(0,0)", "Q(1,0)", "Q(1,1)")),
           groupM = factor(Am, labels = c("No", "Yes")))


  dat1Mb <- dat1Ma |>
    group_by(group, mm) |>
    summarise(Y = mean(Yp2),
              Y_count = sum(Yp2)) |>
    ungroup() |>
    group_by(group) |>
    mutate(Ysum = cumsum(Y_count))|>
    ungroup()

  dat1Mc <- dat1Ma |>
    group_by(group) |>
    summarise(n_id = length(unique(id)))|>
    ungroup()

  dat1M <- dat1Mb |>
    left_join(dat1Mc, by = "group") |>
    mutate(Y_prob_cum = Ysum/n_id)


  ## cumY

  cumy_dfplot <- dat1M |>
    left_join(tv_cumsum, by = join_by(mm == j))

  f_cumY <- cumy_dfplot |>
    ggplot() +
    geom_line(aes(x = mm, y = Y_prob_cum, color = group), linewidth = 1) +
    geom_line(aes(x = mm, y = y_prob_cum), color = "gray20",
              linewidth = 1, linetype = 2) +
    scale_color_brewer(palette = "Set1", direction = -1) +
    labs(title = "Cumulative Y")

  ## tvY

  y_limt2 <- max(cumy_dfplot$Y, cumy_dfplot$y_prop, na.rm=TRUE)

  f_tvY <- cumy_dfplot |>
    ggplot() +
    geom_line(aes(x = mm, y = Y, color = group), linewidth = 1) +
    geom_line(aes(x = mm, y = y_prop), color = "gray20",
              linewidth = 1, linetype = 2) +
    scale_color_brewer(palette = "Set1", direction = -1) +
    labs(title = "Time-varying Y")

  if (what == "cumY") {
    out_plot <- f_cumY
  } else {
    out_plot <- f_tvY
  }

  out_plot
}



