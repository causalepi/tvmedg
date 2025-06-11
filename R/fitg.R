#' Fitting model
#'
#' @param data input data
#' @param boot doing bootstraping
#' @param mreg regression model for mediator
#' @param lreg regression model for time-varying variable
#' @param yreg regression model for outcome variable
#' @param dof degree of freedom
#'
#' @import splines
#' @importFrom dplyr as_tibble
#' @importFrom stats glm
#'
#' @return
#' fitted model
#'
#' @export
fitg <- function(data,boot = FALSE,
                 mreg = "binomial",
                 lreg = c("binomial","gaussian","gaussian"),
                 yreg = "binomial",dof = 3){

  res_df <- resamp(data = data$df,boot = boot)

  fitR <- list()

  fitR$df <- res_df %>% as_tibble()

  #----- fit parametric models for
  #--- Mediator models

  if(length(mreg) != length(data$fm)){
    stop("the defined regression of M is not equal")
  }

  for (i in 1:length(data$fm)){
    fitM <- paste0(data$fm[[i]],"+","splines::bs(jj,df=",dof,")")
    fitR$M[[i]] <- glm(fitM ,family = mreg[i], data = res_df)
  }


  #--- Covariate models
  if(length(lreg) != length(data$fl)){
    stop("the defined regression of L is not equal")
  }

  for (i in 1:length(data$fl)){
    fitL <- paste0(data$fl[[i]],"+","splines::bs(jj,df=",dof,")")
    fitR$L[[i]] <- glm(fitL ,family = lreg[i], data = res_df)
  }

  #--- Outcome model:
  fitY <- paste0(data$fy,"+","splines::bs(jj,df=",dof,")")


  fitR$Y <-  glm(fitY ,family = yreg, data = res_df)

  fitR$norev_var <- data$norev_var

  fitR
}
