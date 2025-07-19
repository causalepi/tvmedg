#' Fitting model
#'
#' @param data input data
#' @param boot doing bootstraping
#' @param mreg regression model for mediator
#' @param lreg regression model for time-varying variable
#' @param yreg regression model for outcome variable
#'
#' @import splines
#' @importFrom dplyr as_tibble
#' @importFrom stats glm
#'
#' @return
#' fitted model
#'
#' @export
fitg <- function(data, boot = FALSE,
                 mreg = NULL,
                 lreg = NULL,
                 yreg = NULL){

  res_df <- resamp(data = data$df, boot = boot)

  fitR <- list()

  fitR$df <- res_df |> as_tibble()

  #----- fit parametric models for
  #--- Mediator models

  if(length(mreg) != length(data$fm)){
    stop("the defined regression of M is not equal")
  }

  for (i in 1:length(data$fm)){
    fitM <- paste0(data$fm[[i]])
    fitR$M[[i]] <- glm(fitM, family = mreg[i], data = fitR$df)
  }


  #--- Covariate models
  if(length(lreg) != length(data$fl)){
    stop("the defined regression of L is not equal")
  }

  for (i in 1:length(data$fl)){
    fitL <- paste0(data$fl[[i]])
    fitR$L[[i]] <- glm(fitL, family = lreg[i], data = fitR$df)
  }

  #--- Outcome model:
  fitY <- paste0(data$fy)
  fitR$Y <-  glm(fitY, family = yreg, data = fitR$df)

  fitR$norev_var <- data$norev_var

  fitR$am <- data$am

  fitR
}


