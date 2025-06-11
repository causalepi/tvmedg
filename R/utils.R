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


#' Extract result
#'
#' @param data input data
#' @importFrom dplyr filter
#' @return
#' Q11,Q10,Q00
#' @export
ExtResult2 <- function(data) {
  Q11 <- data |> filter(lastid == 1 & Ay ==1 & Am ==1)
  Q10 <- data |> filter(lastid == 1 & Ay ==1 & Am ==0)
  Q00 <- data |> filter(lastid == 1 & Ay ==0 & Am ==0)

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



