#' Resampling basic variables
#'
#' @param data input data
#' @param montecarlo number of repeated samples for accept-reject algorithm
#'
#' @return
#' data after resampling
#'
#' @export
baseline_mc <- function(data,montecarlo = 10000){

  boot <- data$df
  # Select baseline visit
  df0 <- boot[boot$j==1, ]

  df0$idn <- 1:nrow(df0)

  MC <- NULL

  samples <- sample(df0$idn, size = montecarlo, replace = T)
  bb <- table(samples)

  for(zzz in 1:max(bb)) {
    cc <- df0[df0$idn %in% names(bb[bb %in% c(zzz:max(bb))]), ]
    cc$bid <- paste0(cc$idn, zzz)
    MC <- rbind(MC, cc)
  }

  MC$idsim <- 1:montecarlo

  data$res_df <- MC %>% as_tibble()
  data
}
