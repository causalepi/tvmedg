#' Time-varying mediation analysis using g-computation
#'
#' @param data input data
#' @param expo exposure variable
#' @param med mediator variable
#' @param tvar time-varying variable
#' @param lag number of lag
#' @param outc outcome variable
#' @param time time variable
#' @param norev non-reversible variable (among expo,med,tvar)
#' @param boot doing boostrap
#' @param seed set seed
#' @param mreg regression model for mediator
#' @param lreg regression model for time-varying variable
#' @param yreg regression model for outcome variable
#' @param montecarlo number of repeated samples for accept-reject algorithm
#' @param parallel run parallel
#' @param nboot bootstraping times
#' @param ci confident interval
#'
#' @param basec  time-fixed variables
#' @param cont_exp continuous exposure
#' @param cont_exp_std standardize the continuous exposure
#' @param tvar_to_med time-varying varible to mediator
#' @param sp_list splines list
#' @param sp_type splines type
#' @param sp_df spines degree of freedom
#' @param followup length of follow up
#'
#' @importFrom dplyr bind_rows
#' @importFrom foreach %dopar% foreach
#'
#' @return
#' Q11, Q10, Q00
#' @export
tvmedg <- function(data, basec, expo, med, tvar, outc, time, lag = 2,
                   norev = NULL, cont_exp = NULL, cont_exp_std = F,
                   tvar_to_med = F,
                   mreg = "binomial",
                   lreg = c("binomial","gaussian","gaussian"),
                   yreg = "binomial",
                   sp_list = NULL, sp_type = NULL, sp_df= NULL,
                   followup = 12,
                   seed = 0, montecarlo = 1000, boot = FALSE, nboot = 1, ci = .95,
                   parallel=TRUE) {

  set.seed(seed)

  start_time <- Sys.time()

  qqq <- matrix(ncol = 3) |> data.frame()
  colnames(qqq) <- c("mQ11","mQ10","mQ00")

  qqq_ci <- matrix(ncol = 3) |> data.frame()
  colnames(qqq_ci) <- c("mQ11","mQ10","mQ00")


  ## point estimate

  fitR2 <- process_data(
    basec = basec,
    expo = expo,
    med = med,
    tvar = tvar,
    outc = outc,
    lag = lag,
    time = time,
    norev = norev,
    tvar_to_med = tvar_to_med,
    cont_exp = cont_exp,
    cont_exp_std = cont_exp_std,
    sp_list = sp_list,
    sp_type = sp_type,
    sp_df = sp_df,
    data = data
  )  |>
    fitg(boot=boot,
         mreg = mreg,
         lreg = lreg,
         yreg = yreg) |>
    baseline_mc(montecarlo = montecarlo)

  am <- fitR2$am


  if (parallel == TRUE){

    resultDatM <- foreach(
      data = fitR2$res_df,
      .combine = rbind,
      .packages = c("splines", "data.table", "dplyr","tvmedg")
    ) %dopar% {
      rbind(
        g_form(data, model = fitR2, followup = followup, ay = am+1, am = am+1),
        g_form(data, model = fitR2, followup = followup, ay = am+1, am = am),
        g_form(data, model = fitR2, followup = followup, ay = am, am = am)
      )
    }

  } else {

    outdat11 <- g_form(data=fitR2$res_df, model = fitR2,followup = followup, ay = am+1, am = am+1)
    outdat10 <- g_form(data=fitR2$res_df, model = fitR2,followup = followup, ay = am+1, am = am)
    outdat00 <- g_form(data=fitR2$res_df, model = fitR2,followup = followup, ay = am, am = am)

    resultDatM <- rbind(outdat11, outdat10, outdat00)

  }

  qqq <- ExtResult2(resultDatM,am = am) |> mutate(
    rIE_b = mQ11 - mQ10,
    rDE_b = mQ10 - mQ00,
    rTE_b = mQ11 - mQ00,
    rPE_b = rIE_b/ rTE_b
  )

  if (boot == TRUE){

    for (it in 1:nboot){

      ## boostrap
      fitR2a <- process_data(
        basec = basec,
        expo = expo,
        med = med,
        tvar = tvar,
        outc = outc,
        lag = lag,
        time = time,
        norev = norev,
        tvar_to_med = tvar_to_med,
        cont_exp = cont_exp,
        cont_exp_std = cont_exp_std,
        sp_list = sp_list,
        sp_type = sp_type,
        sp_df = sp_df,
        data = data)  |>
        fitg(boot = boot,
             mreg = mreg,
             lreg = lreg,
             yreg = yreg) |>
        baseline_mc(montecarlo = montecarlo)

      am_ci <- fitR2a$am

      ## extract mean of q11,q10,q00 of the ith iter
      if (parallel == TRUE){

        resultDatM_ci <- foreach(
          data = fitR2a$res_df,
          .combine = rbind,
          .packages = c("splines", "data.table", "dplyr", "tvmedg")
        ) %dopar% {
          rbind(
            g_form(data, model = fitR2a, followup = followup, ay = am+1, am = am+1),
            g_form(data, model = fitR2a, followup = followup, ay = am+1, am = am),
            g_form(data, model = fitR2a, followup = followup, ay = am, am = am)
          )
        }

      } else {
        outdat11 <- g_form(data=fitR2a$res_df, model = fitR2a,
                           followup = followup, ay = am+1, am = am+1)
        outdat10 <- g_form(data=fitR2a$res_df, model = fitR2a,
                           followup = followup, ay = am+1, am = am)
        outdat00 <- g_form(data=fitR2a$res_df, model = fitR2a,
                           followup = followup, ay = am, am = am)
        resultDatM_ci <- rbind(outdat11, outdat10, outdat00)
      }

      qqq_ci[it,] <- ExtResult2(resultDatM_ci,am = am_ci)
    }

    qqq_ci <- qqq_ci |> mutate(
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
  cat("Q(a,a):", round(qqq$mQ11, 3),cal_ci(qqq_ci$mQ11,ci,boot = boot),'\n')
  cat("Q(a,a*):", round(qqq$mQ10, 3),cal_ci(qqq_ci$mQ10,ci,boot = boot),'\n')
  cat("Q(a*,a*):", round(qqq$mQ00, 3),cal_ci(qqq_ci$mQ00,ci,boot = boot),'\n')

  cat("Indirect:", round(qqq$rIE_b, 3),cal_ci(qqq_ci$rIE_b,ci,boot = boot),'\n')
  cat("Direct:", round(qqq$rDE_b, 3),cal_ci(qqq_ci$rDE_b,ci,boot = boot),'\n')
  cat("Total:", round(qqq$rTE_b, 3),cal_ci(qqq_ci$rTE_b,ci,boot = boot),'\n')
  cat("Proportional explain:",
      round(qqq$rPE_b, 3),cal_ci(qqq_ci$rPE_b,ci,boot = boot),'\n')

  cat("Total time elapsed:",elapsed_time,attr(elapsed_time,"units"),'\n')

  invisible(obj)

}

