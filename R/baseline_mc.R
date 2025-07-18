#' Resampling basic variables
#'
#' @param data input data
#' @param montecarlo number of repeated samples for accept-reject algorithm
#'
#' @return
#' data after resampling
#'
#' @export
baseline_mc <- function(data = fitR2, montecarlo = 1000){

  boot <- as.data.table(data$df)
  df0 <- boot[j == 1]
  df0[, idn := .I]
  samples <- sample(df0$idn, size = montecarlo, replace = TRUE)
  bb <- table(samples)

  MC_list <- lapply(as.integer(names(bb)), function(idn_val) {
    reps <- bb[as.character(idn_val)]
    dt  <- df0[idn == idn_val]
    dt_rep <- dt[rep(1, reps), ]
    dt_rep[, rep := seq_len(reps)]
    dt_rep
  })

  MC <- rbindlist(MC_list, idcol = "idsim")
  MC[, idsim := seq_len(.N)]

  data$res_df <- split(MC, by = "idsim", keep.by = TRUE)
  data
}
