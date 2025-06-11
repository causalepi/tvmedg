#' Centering and scaling time variable
#'
#' @param time time variable
#'
#' @importFrom magrittr %>%
#' @return
#' dataframe with scaled time variable, mean and standard deviation of scaled time variable
#' @export
cen_and_scale <- function(time){

  j_out <- list()

  jj <- time %>%
    scale()

  j_out[["jj"]] <- jj %>% as.numeric()
  j_out[["mean_j"]] <- attributes(jj)$`scaled:center`
  j_out[["sd_j"]] <- attributes(jj)$`scaled:scale`

  j_out
}


#' Resampling input data
#'
#' @param data input data
#' @param boot doing boostrap
#'
#' @return
#' an original dataframe if boot = FALSE, resampled dataframe if boot = TRUE
#' @export
resamp <- function(data,boot = FALSE){

  df <- data

  clusters <- names(table(df$id))
  index <- sample(1:length(clusters), length(clusters), replace = TRUE)
  bb <- table(clusters[index])
  boot_df <- NULL

  if(boot == F) {
    # not doing bootstrap
    boot_df <- df
  } else {
    for(zzz in 1:max(bb)) {
      # Loop over repeated id
      cc <- df[df$id %in% names(bb[bb %in% c(zzz:max(bb))]), ]
      cc$bid <- paste0(cc$id, zzz)
      boot_df <- rbind(boot_df, cc)
    }
  }

  boot_df$jj <- cen_and_scale(boot_df$j)$jj

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








