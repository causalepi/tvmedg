#' Resampling input data
#'
#' @param data input data
#' @param boot doing boostrap
#'
#' @importFrom data.table as.data.table rbindlist copy
#'
#' @return
#' an original dataframe if boot = FALSE, resampled dataframe if boot = TRUE
#' @export
resamp <- function(data,boot = FALSE){

  df <- as.data.table(data)

  # set.seed(seed)
  # cat("Running SEED", seed, "\n")
  # cat("\n")
  # cat("Resampling Data", "\n")

  clusters <- unique(df$id)
  samples  <- sample(clusters, length(clusters), replace = TRUE)
  bb       <- table(samples)

  #— bootstrap
  if (boot == F) {
    # no bootstrap
    boot_df <- copy(df)
  } else {
    maxbb   <- max(bb)
    out_list <- vector("list", maxbb)

    for (zzz in seq_len(maxbb)) {
      # IDs drawn at least zzz times
      ids_zzz <- names(bb)[bb >= zzz]
      cc      <- df[id %in% ids_zzz]
      cc[, bid := paste0(id, zzz)]
      out_list[[zzz]] <- cc
    }

    # one single bind of all “layers”
    boot_df <- rbindlist(out_list, use.names = TRUE)
  }

  boot_df
}


#' predict function
#'
#' @param mod fitted model
#' @param ndat new input data
#'
#' @importFrom stats glm predict rbinom
#' @return
#' predicted value
#' @export
rFunc <- function(mod, ndat) {
  pred_prob <- predict(mod, newdata = ndat, type = "response")
  return(rbinom(1, size = 1, prob = pred_prob))
}


#' Extract result
#'
#' @param am am from model
#' @param data input data
#'
#' @importFrom dplyr filter
#' @return
#' Q11,Q10,Q00
#' @export
ExtResult2 <- function(data,am) {
  Q11 <- data |> filter(lastid == 1 & Ay ==am+1 & Am ==am+1)
  Q10 <- data |> filter(lastid == 1 & Ay ==am+1 & Am ==am)
  Q00 <- data |> filter(lastid == 1 & Ay ==am & Am ==am)

  qq <- data.frame(mQ11 = mean(Q11$Yp2),
                   mQ10 = mean(Q10$Yp2),
                   mQ00 = mean(Q00$Yp2))
  qq
}

#' Confident interval calculation
#'
#' @param data input data
#' @param ci percentage of confident interval
#' @param boot doing bootstraping
#'
#' @importFrom stats quantile
#' @return
#' confident interval
#' @export
cal_ci <- function(data,ci = 0.95,boot = T){

  if (boot == F){
    resu <- NULL
  } else {
    qnt <- quantile(data, na.rm = TRUE, probs = c((1-ci)/2,1 - (1-ci)/2))
    resu <- paste0("(",round(qnt[1],3),",",round(qnt[2],3),")")
  }
  resu
}



