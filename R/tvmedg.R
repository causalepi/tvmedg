#' Time-varying mediation analysis using g-computation
#'
#' @param data input data
#' @param fix time-fixed variable
#' @param expo exposure variable
#' @param med mediator variable
#' @param tvar time-varying variable
#' @param lag number of lag
#' @param outc outcome variable
#' @param time time variable
#' @param norev non-reversible variable (among expo,med,tvar)
#' @param LM L before M assumption
#' @param boot doing boostrap
#' @param seed set seed
#' @param mreg regression model for mediator
#' @param lreg regression model for time-varying variable
#' @param yreg regression model for outcome variable
#' @param dof degree of freedom
#' @param montecarlo number of repeated samples for accept-reject algorithm
#' @param length time to follow up
#' @param parallel run parallel
#' @param nboot bootstraping times
#' @param ci confident interval
#'
#'
#' @importFrom dplyr bind_rows
#' @importFrom furrr future_map_dfr
#' @import future
#' @return
#' Q11, Q10, Q00
#' @export
tvmedg <- function(data,
                   fix,expo,med,tvar,lag,outc,time,norev = NULL,LM = FALSE,
                   boot = FALSE, seed = 0,
                   mreg = "binomial",
                   lreg = c("binomial","gaussian","gaussian"),
                   yreg = "binomial",dof = 3,
                   montecarlo = 10000,length = 12,parallel=TRUE,nboot = 1,ci = .95){

  set.seed(seed)

  start_time <- Sys.time()

  qqq <- matrix(ncol = 3) %>% data.frame()
  colnames(qqq) <- c("mQ11","mQ10","mQ00")

  qqq_ci <- matrix(ncol = 3) %>% data.frame()
  colnames(qqq_ci) <- c("mQ11","mQ10","mQ00")


  ## point estimate

  fitR2 <- process_data(
    fix = fix,
    expo = expo,
    med = med,
    tvar = tvar,
    outc = outc,
    lag = lag,
    time = time,
    norev = norev,
    LM = LM,
    data = data
  )  %>%
    fitg(boot=boot,
         mreg = mreg,
         lreg = lreg,
         yreg = yreg,dof = dof) %>%
    baseline_mc(montecarlo = montecarlo)

  if (parallel == TRUE){

    gform_wrapper2 <- function(iii, data, length) {
      outdat11 <- g_form(ii = iii,data=data, length = length, ay = 1, am = 1)
      outdat10 <- g_form(ii = iii,data=data, length = length, ay = 1, am = 0)
      outdat00 <- g_form(ii = iii,data=data, length = length, ay = 0, am = 0)
      bind_rows(outdat11, outdat10, outdat00)
    }

    resultDatM <- future_map_dfr(1:montecarlo,
                                 ~ gform_wrapper2(ii = .x, data = fitR2, length = length))
  } else {
    resultDatM <- data.frame()
    for (iii in 1:montecarlo){
      outdat11 <- g_form(ii = iii,data=fitR2, length = length, ay = 1, am = 1)
      outdat10 <- g_form(ii = iii,data=fitR2, length = length, ay = 1, am = 0)
      outdat00 <- g_form(ii = iii,data=fitR2, length = length, ay = 0, am = 0)
      resultDatM2 <- rbind(outdat11, outdat10, outdat00)
      resultDatM <- rbind(resultDatM,resultDatM2)
    }
  }

  qqq <- ExtResult2(resultDatM) %>% mutate(
    rIE_b = mQ11 - mQ10,
    rDE_b = mQ10 - mQ00,
    rTE_b = mQ11 - mQ00,
    rPE_b = rIE_b/ rTE_b
  )

  if (boot == TRUE){

    for (it in 1:nboot){

      ## boostrap
      fitR2a <- process_data(
        fix = fix,
        expo = expo,
        med = med,
        tvar = tvar,
        outc = outc,
        lag = lag,
        time = time,
        norev = norev,
        LM = LM,
        data = data
      )  %>%
        fitg(boot=boot,
             mreg = mreg,
             lreg = lreg,
             yreg = yreg,dof = dof) %>%
        baseline_mc(montecarlo = montecarlo)

      ## extract mean of q11,q10,q00 of the ith iter
      if (parallel == TRUE){

        gform_wrapper2 <- function(iii, data, length) {
          outdat11 <- g_form(ii = iii,data=data, length = length, ay = 1, am = 1)
          outdat10 <- g_form(ii = iii,data=data, length = length, ay = 1, am = 0)
          outdat00 <- g_form(ii = iii,data=data, length = length, ay = 0, am = 0)
          bind_rows(outdat11, outdat10, outdat00)
        }

        resultDatM_ci <- future_map_dfr(1:montecarlo,
                                        ~ gform_wrapper2(ii = .x, data = fitR2a, length = length))
      } else {
        resultDatM <- data.frame()
        for (iii in 1:montecarlo){
          outdat11 <- g_form(ii = iii,data=fitR2a, length = length, ay = 1, am = 1)
          outdat10 <- g_form(ii = iii,data=fitR2a, length = length, ay = 1, am = 0)
          outdat00 <- g_form(ii = iii,data=fitR2a, length = length, ay = 0, am = 0)
          resultDatM2_ci <- rbind(outdat11, outdat10, outdat00)
          resultDatM_ci <- rbind(resultDatM_ci,resultDatM2_ci)
        }
      }

      qqq_ci[it,] <- ExtResult2(resultDatM_ci)
    }

    qqq_ci <- qqq_ci %>% mutate(
      rIE_b = mQ11 - mQ10,
      rDE_b = mQ10 - mQ00,
      rTE_b = mQ11 - mQ00,
      rPE_b = rIE_b/ rTE_b
    )

  }


  end_time <- Sys.time()
  elapsed_time <- end_time - start_time

  obj <- list()

  obj$ori_df <- fitR2$df
  obj$dat_MC <- resultDatM
  class(obj) <- "tvmedg"

  ## print result
  cat("Q(1,1):", round(qqq$mQ11, 3),cal_ci(qqq_ci$mQ11,ci,boot = boot),'\n')
  cat("Q(1,0):", round(qqq$mQ10, 3),cal_ci(qqq_ci$mQ10,ci,boot = boot),'\n')
  cat("Q(0,0):", round(qqq$mQ00, 3),cal_ci(qqq_ci$mQ00,ci,boot = boot),'\n')

  cat("Indirect:", round(qqq$rIE_b, 3),cal_ci(qqq_ci$rIE_b,ci,boot = boot),'\n')
  cat("Direct:", round(qqq$rDE_b, 3),cal_ci(qqq_ci$rDE_b,ci,boot = boot),'\n')
  cat("Total:", round(qqq$rTE_b, 3),cal_ci(qqq_ci$rTE_b,ci,boot = boot),'\n')
  cat("Proportional explain:",
      round(qqq$rPE_b, 3),cal_ci(qqq_ci$rPE_b,ci,boot = boot),'\n')

  cat("Total time elapsed:",elapsed_time,attr(elapsed_time,"units"),'\n')

  invisible(obj)

}
