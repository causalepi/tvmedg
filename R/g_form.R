#' g-formula
#'
#' @param data input data
#' @param am am
#' @param ay ay
#' @param model model for g formula
#' @param followup length of follow up
#'
#' @importFrom dplyr contains matches case_when
#'
#' @return
#' dataframe with predict to follow up time
#' @export
g_form <- function(data = fitR2$res_df, model = fitR2, followup, am = 1, ay = 0){

  norev_var <- model$norev_var

  dddd   <- data |> as.data.frame()
  id     <- dddd$idsim[1]
  id_ori <- dddd$id[1]

  lagg <- dddd |> dplyr::select(contains("L1l")) |> ncol()

  # Baseline covariates
  Vp <- dddd |> select(starts_with("v"))

  Yp2 <- numeric(followup)

  mm <- seq_len(followup)


  Yp2[1:lagg-1] <- 0


  # mediator
  Mp <- matrix(ncol = length(model$M)) |> data.frame()
  names(Mp) <- paste0("M",1:length(model$M))
  Mp[1:lagg-1,] <- dddd |> select(names(Mp))


  # time-varying covariates (contribute to mediator models)
  Lmp <- matrix(ncol = length(model$L)) |> data.frame()
  names(Lmp) <- paste0("L",1:length(model$L))
  Lmp[1:lagg-1,] <- dddd |> select(names(Lmp))
  # time-varying covariates (contribute to outcome models)
  Lp <- Lmp

  for (l in lagg:followup) {

    if (Yp2[l-1]==1) {
      # event occurred at time (l-1) → stop here
      actual_length <- l - 1
      break
    } else{

      # Predict mediator
      var_fm <- attr(model$M[[1]]$terms, "term.labels")

      var_fm <- gsub(".*\\(([^,]+),.*", "\\1", var_fm)

      var_fm <- var_fm[-length(var_fm)]

      dfMp <- dddd |> select(matches(var_fm)) |>
        mutate(j = l)

      dfMp[startsWith(colnames(dfMp), "A")] <- am


      if (l > lagg){

        for (zz in 1:(lagg)){
          term <- paste0("l",zz)
          # L lag
          dfMp[startsWith(colnames(dfMp), "L") & endsWith(colnames(dfMp), term)] <- Lmp[l-zz,]
          # M lag
          dfMp[startsWith(colnames(dfMp), "M") & endsWith(colnames(dfMp), term)] <- Mp[l-zz,]
        }

      }


      for (x in 1:length(model$M)){

        M_reg <- model$M[[x]]$family$family

        if (names(Mp[x]) %in% norev_var){

          if (M_reg == "binomial" & Mp[l-1,x] == 1) {
            Mp[l,x] <- 1
          } else {
            Mp[l,x] <- case_when(
              M_reg  == "binomial" ~ rFunc(model$M[[x]], dfMp),
              M_reg  == "gaussian" ~ predict(model$M[[x]], dfMp)
            )
          }

        } else {

          Mp[l,x] <- case_when(
            M_reg  == "binomial" ~ rFunc(model$M[[x]], dfMp),
            M_reg  == "gaussian" ~ predict(model$M[[x]], dfMp)
          )

        }

      }

      # Predict time-varying covariates (contribute to mediator models)
      # L
      var_fl <- attr(model$L[[1]]$terms, "term.labels")
      var_fl <- gsub(".*\\(([^,]+),.*", "\\1", var_fl)
      var_fl <- var_fl[-length(var_fl)]
      dfLmp <- dddd |> select(matches(var_fl)) |>
        mutate(j = l)
      dfLmp[startsWith(colnames(dfLmp), "A")] <- am
      dfLmp[colnames(dfLmp) == colnames(Mp)] <- Mp[l,]


      if (l > lagg){

        for (zz in 1:lagg){
          term <- paste0("l",zz)
          # L lag
          dfLmp[startsWith(colnames(dfLmp), "L") & endsWith(colnames(dfLmp), term)] <- Lmp[l-zz,]
          # M lag
          dfLmp[startsWith(colnames(dfLmp), "M") & endsWith(colnames(dfLmp), term)] <- Mp[l-zz,]
        }

      }

      for (x in 1:length(model$L)){

        L_reg <- model$L[[x]]$family$family

        if (names(Lmp[x]) %in% norev_var){
          if (L_reg == "binomial" & Lmp[l-1,x] == 1){

            Lmp[l,x] <- 1

          } else {

            Lmp[l,x] <- case_when(
              L_reg  == "binomial" ~ rFunc(model$L[[x]], dfLmp),
              L_reg  == "gaussian" ~ predict(model$L[[x]], dfLmp)
            )

          }

        } else{

          Lmp[l,x] <- case_when(
            L_reg  == "binomial" ~ rFunc(model$L[[x]], dfLmp),
            L_reg  == "gaussian" ~ predict(model$L[[x]], dfLmp)
          )

        }

      }

      # Predict time-varying covariates (contribute to outcome models, if ay != am)
      if (ay != am) {
        dfLp <- dddd |> select(matches(var_fl)) |>
          mutate(j = l)
        dfLp[startsWith(colnames(dfLp), "A")] <- ay
        dfLp[colnames(dfLp) == colnames(Mp)] <- Mp[l,]

        if (l > lagg){

          for (zz in 1:lagg){
            term <- paste0("l",zz)
            # L lag
            dfLp[startsWith(colnames(dfLp), "L") & endsWith(colnames(dfLp), term)] <- Lp[l-zz,]
            # M lag
            dfLp[startsWith(colnames(dfLp), "M") & endsWith(colnames(dfLp), term)] <- Mp[l-zz,]
          }

        }

        for (x in 1:length(model$L)){

          L_reg <- model$L[[x]]$family$family

          if (names(Lp[x]) %in% norev_var){
            if (L_reg == "binomial" & Lp[l-1,x] == 1){

              Lp[l,x] <- 1

            } else {

              Lp[l,x] <- case_when(
                L_reg  == "binomial" ~ rFunc(model$L[[x]], dfLp),
                L_reg  == "gaussian" ~ predict(model$L[[x]], dfLp)
              )

            }

          } else {

            Lp[l,x] <- case_when(
              L_reg  == "binomial" ~ rFunc(model$L[[x]], dfLp),
              L_reg  == "gaussian" ~ predict(model$L[[x]], dfLp)
            )

          }

        }

      } else{
        Lp <- Lmp
      }

      # Y
      var_y <- attr(model$Y$terms, "term.labels")
      var_y <- gsub(".*\\(([^,]+),.*", "\\1", var_y)
      var_y <- var_y[-length(var_y)]
      dfYp <- dddd |> select(matches(var_y)) |>
        mutate(j = l)
      dfYp[startsWith(colnames(dfYp), "A")] <- ay
      dfYp[colnames(dfYp) %in% colnames(Mp)] <- Mp[l,]
      dfYp[colnames(dfYp) %in% colnames(Lp)] <- Lp[l,]

      if (l > lagg){

        for (zz in 1:lagg){
          term <- paste0("l",zz)
          # L lag
          dfYp[startsWith(colnames(dfYp), "L") & endsWith(colnames(dfYp), term)] <- Lp[l-zz,]
          # M lag
          dfYp[startsWith(colnames(dfYp), "M") & endsWith(colnames(dfYp), term)] <- Mp[l-zz,]
        }

      }

      Yp2[l] <- rFunc(model$Y, dfYp)

    }

    mm[l] <- l
  }

  if (!exists("actual_length")) {
    actual_length <- followup
  }

  colnames(Lmp) <- paste0("Lmp",1:length(model$L))
  colnames(Lp) <- paste0("Lp",1:length(model$L))

  mm <- mm[1:actual_length]
  Yp2 <- Yp2[1:actual_length]

  Mp <- Mp[1:actual_length,]
  Lmp <- Lmp[1:actual_length,]
  Lp <- Lp[1:actual_length,]

  # boot_num <- seed
  gdat2 <- data.frame(id, id_ori, mm, Ay = ay, Am = am, Mp, Yp2,
                      Lmp, Lp, Vp)
  gdat2$lastid <- as.numeric(!duplicated(gdat2$id, fromLast = T))

  return(gdat2)
}

