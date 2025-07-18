library(tvmedg)
library(dplyr)
library(splines)
library(data.table)
library(foreach)

df_prep <- function(data) {
  data.frame(
    id   = data$id,
    v1   = data$age,
    v2   = data$sex,
    v3   = data$ow,
    v4   = data$risk,
    A    = data$Ap,
    Al1  = data$A_lag1,
    Al2  = data$A_lag2,
    T1   = data$L1,
    T1l1 = data$L1_lag1,
    T1l2 = data$L1_lag2,
    T2   = data$L2,
    T2l1 = data$L2_lag1,
    T2l2 = data$L2_lag2,
    T3   = data$L3,
    T3l1 = data$L3_lag1,
    T3l2 = data$L3_lag2,
    M1   = data$Mp,
    M1l1 = data$M_lag1,
    M1l2 = data$M_lag2,
    Y    = data$Yp,
    j    = data$mm
  )
}

gform_single <- function(subdat = MC_list2, length, fitR, ay, am) {

  rFunc <- function(mod, ndat) {
    pred_prob <- predict(mod, newdata = ndat, type = "response")
    return(rbinom(1, size = 1, prob = pred_prob))
  }

  id     <- subdat$idsim[1]
  id_ori <- subdat$id[1]


  mm <- seq_len(length)

  Yp   <- numeric(length)

  M1p  <- numeric(length)

  T1p  <- numeric(length)
  T2p  <- numeric(length)
  T3p  <- numeric(length)

  T1mp <- numeric(length)
  T2mp <- numeric(length)
  T3mp <- numeric(length)

  # Baseline covariates
  Vp      <- subdat[1, c("v1","v2","v3","v4")]

  M1p[1] <- 0
  Yp[1]  <- 0

  # Time-varying covariates contribute to outcome model
  T1p[1] <- subdat$T1
  T2p[1] <- subdat$T2
  T3p[1] <- subdat$T3

  # Time-varying covariates contribute to mediator model
  T1mp[1] <- subdat$T1
  T2mp[1] <- subdat$T2
  T3mp[1] <- subdat$T3



  for (l in 2:length) {

    if (Yp[l-1] == 1) {
      # event occurred at time (l-1) → stop here
      actual_length <- l - 1
      break
    }

    # compute 1- and 2-step lags
    i1  <- max(1, l-1)
    i2  <- max(1, l-2)

    T1l1  <- T1p[i1]
    T2l1  <- T2p[i1]
    T3l1  <- T3p[i1]

    T1l2  <- T1p[i2]
    T2l2  <- T2p[i2]
    T3l2  <- T3p[i2]

    T1ml1 <- T1mp[i1]
    T2ml1 <- T2mp[i1]
    T3ml1 <- T3mp[i1]

    T1ml2 <- T1mp[i2]
    T2ml2 <- T2mp[i2]
    T3ml2 <- T3mp[i2]

    M1l1  <- M1p[i1]
    M1l2  <- M1p[i2]

    # Predict mediator
    dM1p <- data.frame(Vp, A = am, Al1 = am, Al2 = am, M1l1, M1l2, T1l1 = T1ml1, T1l2 = T1ml2,
                       T2l1 = T2ml1, T2l2 = T2ml2, T3l1 = T3ml1, T3l2 = T3ml2,
                       j = l)

    M1p[l] <- if (M1p[l-1] == 0) rFunc(fitR[[1]], dM1p) else 1


    # (2) covariates for mediator model
    dT1mp <- transform(dM1p, M1 = M1p[l])
    T1mp[l] <- rFunc(fitR[[3]], dT1mp)

    dT2mp <- transform(dT1mp, T1 = T1mp[l])
    T2mp[l] <- predict(fitR[[4]], newdata = dT2mp)

    dT3mp <- transform(dT2mp, T2 = T2mp[l])
    T3mp[l] <- predict(fitR[[5]], newdata = dT3mp)

    if (ay != am) {
      dT1p   <- transform(dM1p, A = ay, Al1 = ay, Al2 = ay, M1 = M1p[l],
                          T1l1  = T1p[i1],T2l1  = T2p[i1],T3l1  = T3p[i1],
                          T1l2  = T1p[i2],T2l2  = T2p[i2],T3l2  = T3p[i2])

      T1p[l] <- rFunc(fitR[[3]], dT1p)

      dT2p   <- transform(dT1p, T1 = T1p[l])
      T2p[l] <- predict(fitR[[4]], newdata = dT2p)

      dT3p   <- transform(dT2p, T2 = T2p[l])
      T3p[l] <- predict(fitR[[5]], newdata = dT3p)

    } else {
      T1p[l] <- T1mp[l]
      T2p[l] <- T2mp[l]
      T3p[l] <- T3mp[l]
    }

    # Y
    dYp <- data.frame(Vp, A = ay, Al1 = ay, Al2 = ay, M1 = M1p[l], M1l1, M1l2,
                      T1 = T1p[l], T1l1, T1l2,
                      T2 = T2p[l], T2l1, T2l2,
                      T3 = T3p[l], T3l1, T3l2,
                      j = l)
    Yp[l] <- rFunc(fitR[[2]], dYp)
  }

  if (!exists("actual_length")) {
    actual_length <- length
  }


  mm   <- mm[1:actual_length]
  M1p  <- M1p[1:actual_length]
  Yp   <- Yp[1:actual_length]
  T1p  <- T1p[1:actual_length]
  T2p  <- T2p[1:actual_length]
  T3p  <- T3p[1:actual_length]
  T1mp <- T1mp[1:actual_length]
  T2mp <- T2mp[1:actual_length]
  T3mp <- T3mp[1:actual_length]

  gdat <- data.frame(id, id_ori, mm, Ay = ay, Am = am, M1p, Yp,
                     T1mp, T1p, T2mp, T2p, T3mp, T3p, Vp)
  gdat$lastid <- as.numeric(!duplicated(gdat$id, fromLast = T))

  return(gdat)
}


dat <- sim_data |>
  group_by(id) |>
  mutate(
    A_lag1 = lag(Ap, n = 1, default = 0),
    A_lag2 = lag(Ap, n = 2, default = 0),
    M_lag1 = lag(Mp, n = 1, default = 0),
    M_lag2 = lag(Mp, n = 2, default = 0),
    L1_lag1 = lag(L1, n = 1, default = 0),
    L1_lag2 = lag(L1, n = 2, default = 0),
    L2_lag1 = lag(L2, n = 1, default = 100),
    L2_lag2 = lag(L2, n = 2, default = 100),
    L3_lag1 = lag(L3, n = 1, default = 80),
    L3_lag2 = lag(L3, n = 2, default = 80)
  ) |> ungroup()

data = dat
montecarlo= 100
seed = 0

df <- as.data.table(df_prep(data))

set.seed(seed)

# Resampling based on id and store in `boot` dataset
clusters <- unique(df$id)
samples  <- sample(clusters, length(clusters), replace = TRUE)
bb       <- table(samples)

#— bootstrap
if (seed == 0) {
  # no bootstrap
  boot <- copy(df)
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
  boot <- rbindlist(out_list, use.names = TRUE)
}

#----- fit parametric models for
#--- Mediator models

fitM1 <- glm(M1 ~ A + Al1 + Al2 + M1l1 + M1l2 + T1l1 + T1l2 + T2l1 + T2l2 + T3l1 + T3l2 +
               v1 + v2 + v3 + v4 + bs(j, df = 3),
             family = binomial, data = boot) |> suppressWarnings()

# PseudoR2(fitM1)
#--- Covariate models

fitT1 <- glm(T1 ~ A + Al1 + Al2 + M1 + M1l1 + M1l2 + T1l1 + T1l2 + T2l1 + T2l2 + T3l1 + T3l2 +
               v1 + v2 + v3 + v4 + bs(j, df = 3),
             family = binomial, data = boot) |> suppressWarnings()

# PseudoR2(fitT1)

fitT2 <- lm(T2 ~ A + Al1 + Al2 + M1 + M1l1 + M1l2 + T1 + T1l1 + T1l2 + T2l1 + T2l2 + T3l1 + T3l2 +
              v1 + v2 + v3 + v4 + bs(j, df = 3), data = boot) |> suppressWarnings()

fitT3 <- lm(T3 ~ A + Al1 + Al2 + M1 + M1l1 + M1l2 + T1 + T1l1 + T1l2 + T2l1 + T2l2 + T3l1 + T3l2 +
              v1 + v2 + v3 + v4 + bs(j, df = 3), data = boot) |> suppressWarnings()

# Outcome model: E(Y|a, m, l, v)

fitY <- glm(Y ~ A + Al1 + Al2 + M1 + M1l1 + M1l2 + T1 + T1l1 + T1l2 + T2 + T2l1 + T2l2 +
              T3 + T3l1 + T3l2 +  v1 + v2 + v3 + v4 + bs(j, df = 3),
            family = binomial, data = boot) |> suppressWarnings()

fitR <- list(fitM1, fitY, fitT1, fitT2, fitT3)

# Avoid growing object

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


# split once (Key improvement)
MC_list2 <- split(MC, by = "idsim", keep.by = TRUE)

length =12
result <- suppressWarnings(foreach(
  subdat   = MC_list2,
  .combine = rbind,
  .packages = c("splines", "data.table"),
  .export   = c("gform_single")
) %dopar% {
  rbind(
    gform_single(subdat, length, fitR, ay = 1, am = 1),
    gform_single(subdat, length, fitR, ay = 1, am = 0),
    gform_single(subdat, length, fitR, ay = 0, am = 0)
  )
})

## package
set.seed(123)
fitR2 <- process_data(
  basec = c("age","sex","ow","risk"),
  expo = c("Ap"),
  med = c("Mp"),
  tvar = c("L1","L2","L3"),
  outc = c("Yp"),
  lag = 2,
  time = c("mm"),
  norev = c("Mp"),
  tvar_to_med = F,
  cont_exp = F,
  cont_exp_std = F,
  sp_list = c("mm"),
  sp_type = c("bs"),
  sp_df= c(3),
  data = sim_data
) |> fitg(boot=F,
          mreg = "binomial",
          lreg = c("binomial","gaussian","gaussian"),
          yreg = "binomial") |>
  baseline_mc(montecarlo = 100) |> suppressWarnings()

resultDatM <-  foreach(
  data = fitR2$res_df,
  .combine = rbind,
  .packages = c("splines", "data.table", "dplyr","tvmedg"),
  .export = "g_form"
) %dopar% {
  rbind(
    g_form(data, model = fitR2, followup = 12, ay = 1, am = 1),
    g_form(data, model = fitR2, followup = 12, ay = 1, am = 0),
    g_form(data, model = fitR2, followup = 12, ay = 0, am = 0)
  )
} |> suppressWarnings()


## am = ay = 1

along_redf <- result |> filter(Am == 1 & Ay == 1 & id_ori == 4)
test_redf <- resultDatM |> filter(Am == 1 & Ay ==1 & id_ori == 4)
test_that("gform function", {

  # boot_num - Am
  expect_equal(along_redf[,c(1:5)],test_redf[,c(1:5)])
  expect_equal(along_redf[,c("v1","v2","v3","v4","lastid")],
               test_redf[,c("v1","v2","v3","v4","lastid")])

  expect_equal(along_redf[,c("M1p")],test_redf[,c("Mp")])
  expect_equal(along_redf[,c("T1mp")],test_redf[,c("Lmp1")])
  expect_equal(along_redf[,c("T1p")],test_redf[,c("Lp1")])
  expect_equal(along_redf[,c("T2mp")],test_redf[,c("Lmp2")])
  expect_equal(along_redf[,c("T2p")],test_redf[,c("Lp2")])
  expect_equal(along_redf[,c("T3mp")],test_redf[,c("Lmp3")])
  expect_equal(along_redf[,c("T3p")], test_redf[,c("Lp3")])
  expect_equal(along_redf[,c("Yp")], test_redf[,c("Yp2")])
})

## am = ay = 0
along_redf <- result |> filter(Am == 0 & Ay == 0 & id_ori == 4)
test_redf <- resultDatM |> filter(Am == 0 & Ay ==0 & id_ori == 4)
test_that("gform function", {

  # boot_num - Am
  expect_equal(along_redf[,c(1:5)],test_redf[,c(1:5)])
  expect_equal(along_redf[,c("v1","v2","v3","v4","lastid")],
               test_redf[,c("v1","v2","v3","v4","lastid")])

  expect_equal(along_redf[,c("M1p")],test_redf[,c("Mp")])
  expect_equal(along_redf[,c("T1mp")],test_redf[,c("Lmp1")])
  expect_equal(along_redf[,c("T1p")],test_redf[,c("Lp1")])
  expect_equal(along_redf[,c("T2mp")],test_redf[,c("Lmp2")])
  expect_equal(along_redf[,c("T2p")],test_redf[,c("Lp2")])
  expect_equal(along_redf[,c("T3mp")],test_redf[,c("Lmp3")])
  expect_equal(along_redf[,c("T3p")], test_redf[,c("Lp3")])
  expect_equal(along_redf[,c("Yp")], test_redf[,c("Yp2")])
})

## am = 1, ay = 0
along_redf <- result |> filter(Am == 0 & Ay == 1 & id_ori == 4)
test_redf <- resultDatM |> filter(Am == 0 & Ay ==1 & id_ori == 4)
test_that("gform function", {

  # boot_num - Am
  expect_equal(along_redf[,c(1:5)],test_redf[,c(1:5)])
  expect_equal(along_redf[,c("v1","v2","v3","v4","lastid")],
               test_redf[,c("v1","v2","v3","v4","lastid")])

  expect_equal(along_redf[,c("M1p")],test_redf[,c("Mp")])
  expect_equal(along_redf[,c("T1mp")],test_redf[,c("Lmp1")])
  expect_equal(along_redf[,c("T1p")],test_redf[,c("Lp1")])
  expect_equal(along_redf[,c("T2mp")],test_redf[,c("Lmp2")])
  expect_equal(along_redf[,c("T2p")],test_redf[,c("Lp2")])
  expect_equal(along_redf[,c("T3mp")],test_redf[,c("Lmp3")])
  expect_equal(along_redf[,c("T3p")], test_redf[,c("Lp3")])
  expect_equal(along_redf[,c("Yp")], test_redf[,c("Yp2")])
})
