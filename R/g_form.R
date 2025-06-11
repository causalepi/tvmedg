#' g-formula
#'
#' @param data input data
#' @param ii patient id
#' @param length time to follow up
#' @param am am
#' @param ay ay
#'
#' @importFrom dplyr contains matches case_when
#'
#' @return
#' dataframe with predict to follow up time
#' @export
g_form <- function(data, ii = 2, length = 12, am = 1, ay = 0){

  norev_var <- data$norev_var

  dddd <- data$res_df %>% data.frame()

  lagg <- dddd %>% dplyr::select(contains("L1l")) %>% ncol()

  d2 <- dddd[dddd$idsim==ii, ]

  id <- d2$idsim
  id_ori <- d2$id

  length <- length

  # Baseline covariates
  Vp <- d2 %>% select(starts_with("v"))

  Yp2 <- mm <- numeric()

  mm[1:lagg-1] <- j <- 1

  Yp2[1:lagg-1] <- 0

  timee <- cen_and_scale(data$df$j)

  # mediator
  Mp <- matrix(ncol = length(data$M)) %>% data.frame()
  names(Mp) <- paste0("M",1:length(data$M))
  Mp[1:lagg-1,] <- d2 %>% select(names(Mp))


  # time-varying covariates (contribute to mediator models)
  Lmp <- matrix(ncol = length(data$L)) %>% data.frame()
  names(Lmp) <- paste0("L",1:length(data$L))
  Lmp[1:lagg-1,] <- d2 %>% select(names(Lmp))
  # time-varying covariates (contribute to outcome models)
  Lp <- Lmp

  for (l in lagg:length) {

    if (Yp2[l-1]==1) {
      break
    } else{

      # Predict mediator
      var_fm <- attr(data$M[[1]]$terms, "term.labels")
      var_fm <- var_fm[-length(var_fm)]

      dfMp <- d2 %>% select(matches(var_fm)) %>%
        mutate(jj = (l-timee$mean_j)/timee$sd_j)

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


      for (x in 1:length(data$M)){

        M_reg <- data$M[[x]]$family$family

        if (names(Mp[x]) %in% norev_var){

          if (M_reg == "binomial" & Mp[l-1,x] == 1) {
            Mp[l,x] <- 1
          } else {
            Mp[l,x] <- case_when(
              M_reg  == "binomial" ~ rFunc(data$M[[x]], dfMp),
              M_reg  == "gaussian" ~ predict(data$M[[x]], dfMp)
            )
          }

        } else {

          Mp[l,x] <- case_when(
            M_reg  == "binomial" ~ rFunc(data$M[[x]], dfMp),
            M_reg  == "gaussian" ~ predict(data$M[[x]], dfMp)
          )

        }

      }

      # Predict time-varying covariates (contribute to mediator models)
      # L
      var_fl <- attr(data$L[[1]]$terms, "term.labels")
      var_fl <- var_fl[-length(var_fl)]
      dfLmp <- d2 %>% select(matches(var_fl)) %>%
        mutate(jj = (l-timee$mean_j)/timee$sd_j)
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

      for (x in 1:length(data$L)){

        L_reg <- data$L[[x]]$family$family

        if (names(Lmp[x]) %in% norev_var){
          if (L_reg == "binomial" & Lmp[l-1,x] == 1){

            Lmp[l,x] <- 1

          } else {

            Lmp[l,x] <- case_when(
              L_reg  == "binomial" ~ rFunc(data$L[[x]], dfLmp),
              L_reg  == "gaussian" ~ predict(data$L[[x]], dfLmp)
            )

          }

        } else{

          Lmp[l,x] <- case_when(
            L_reg  == "binomial" ~ rFunc(data$L[[x]], dfLmp),
            L_reg  == "gaussian" ~ predict(data$L[[x]], dfLmp)
          )

        }

      }

      # Predict time-varying covariates (contribute to outcome models, if ay != am)
      if (ay != am) {
        dfLp <- d2 %>% select(matches(var_fl)) %>%
          mutate(jj = (l-timee$mean_j)/timee$sd_j)
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

        for (x in 1:length(data$L)){

          L_reg <- data$L[[x]]$family$family

          if (names(Lp[x]) %in% norev_var){
            if (L_reg == "binomial" & Lp[l-1,x] == 1){

              Lp[l,x] <- 1

            } else {

              Lp[l,x] <- case_when(
                L_reg  == "binomial" ~ rFunc(data$L[[x]], dfLp),
                L_reg  == "gaussian" ~ predict(data$L[[x]], dfLp)
              )

            }

          } else {

            Lp[l,x] <- case_when(
              L_reg  == "binomial" ~ rFunc(data$L[[x]], dfLp),
              L_reg  == "gaussian" ~ predict(data$L[[x]], dfLp)
            )

          }

        }

      } else{
        Lp <- Lmp
      }

      # Y
      var_y <- attr(data$Y$terms, "term.labels")
      var_y <- var_y[-length(var_y)]
      dfYp <- d2 %>% select(matches(var_y)) %>%
        mutate(jj = (l-timee$mean_j)/timee$sd_j)
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

      Yp2[l] <- rFunc(data$Y, dfYp)

    }

    mm[l] <- l
  }

  colnames(Lmp) <- paste0("Lmp",1:length(data$L))
  colnames(Lp) <- paste0("Lp",1:length(data$L))

  # boot_num <- seed
  gdat2 <- data.frame(id, id_ori, mm, Ay = ay, Am = am, Mp, Yp2,
                      Lmp, Lp, Vp)
  gdat2$lastid <- as.numeric(!duplicated(gdat2$id, fromLast = T))
  return(gdat2)
}
