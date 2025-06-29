## manual

df_prep <- function(data) {

  df <- data.frame(id = data$id)

  # Baseline characteristics
  df$v1 <- data$age
  df$v2 <- data$sex
  df$v3 <- data$ow
  df$v4 <- data$risk


  # Time-varying treatment
  df$A <- data$Ap
  df$Al1 <- data$A_lag1
  df$Al2 <- data$A_lag2


  # time-dependent covariate
  df$T1 <- data$L1
  df$T1l1 <- data$L1_lag1
  df$T1l2 <- data$L1_lag2

  df$T2 <- data$L2
  df$T2l1 <- data$L2_lag1
  df$T2l2 <- data$L2_lag2

  df$T3 <- data$L3
  df$T3l1 <- data$L3_lag1
  df$T3l2 <- data$L3_lag2

  # Mediator
  df$M1 <- data$Mp
  df$M1l1 <- data$M_lag1
  df$M1l2 <- data$M_lag2

  # outcomes
  df$Y <- data$Yp

  # Time
  df$j <- data$mm

  return(df)
}

gform <- function(ii, pgdat, length, am, ay) {

  pFunc <- function(mod, ndat) {
    as.numeric(predict(mod, newdata = ndat, type = "response") > runif(1))
  }

  rFunc <- function(mod, ndat) {
    pred_prob <- predict(mod, newdata = ndat, type = "response")
    return(rbinom(1, size = 1, prob = pred_prob))
  }

  d <- pgdat
  d <- d[d$idsim==ii, ]

  id <- d$idsim
  id_ori <- d$id

  length <- length


  # cat("...", paste0(ii, "(", id_ori, ")"))



  # Baseline covariates
  Vp <- d[, c("v1", "v2", "v3", "v4")]

  T1p <- T2p <- T3p  <- M1p <- Yp <- mm <- numeric()
  T1mp <- T2mp <- T3mp <- numeric()

  mm[1] <- j <- 1


  M1p[1] <- 0
  Yp[1] <- 0

  # Time-varying covariates contribute to outcome model
  T1p[1] <- d$T1
  T2p[1] <- d$T2
  T3p[1] <- d$T3

  # Time-varying covariates contribute to mediator model
  T1mp[1] <- d$T1
  T2mp[1] <- d$T2
  T3mp[1] <- d$T3


  for (l in 2:length) {

    if (Yp[l-1]==0) {

      if (l == 2) {
        T1l2 <- T1p[1]
        T2l2 <- T2p[1]
        T3l2 <- T3p[1]

        T1ml2 <- T1mp[1]
        T2ml2 <- T2mp[1]
        T3ml2 <- T3mp[1]
        M1l2 <- M1p[1]

      } else {

        T1l2 <- T1p[l-2]
        T2l2 <- T2p[l-2]
        T3l2 <- T3p[l-2]
        T1ml2 <- T1mp[l-2]
        T2ml2 <- T2mp[l-2]
        T3ml2 <- T3mp[l-2]
        M1l2 <- M1p[l-2]

      }

      T1l1 <- T1p[l-1]
      T2l1 <- T2p[l-1]
      T3l1 <- T3p[l-1]
      T1ml1 <- T1mp[l-1]
      T2ml1 <- T2mp[l-1]
      T3ml1 <- T3mp[l-1]

      M1l1 <- M1p[l-1]



      # Predict mediator
      dM1p <- data.frame(Vp, A = am, Al1 = am, Al2 = am, M1l1, M1l2, T1l1 = T1ml1, T1l2 = T1ml2,
                         T2l1 = T2ml1, T2l2 = T2ml2, T3l1 = T3ml1, T3l2 = T3ml2,
                         jj = as.numeric((l-mean_j)/sd_j))

      if (M1p[l - 1] == 0) {
        # M1p[l] <- pFunc(fitR[[1]], dM1p)
        M1p[l] <- rFunc(fitR[[1]], dM1p)
      } else {
        M1p[l] <- 1
      }



      # Predict time-varying covariates (contribute to mediator models)
      # T1
      dT1mp <- data.frame(Vp, A = am, Al1 = am, Al2 = am,
                          M1 = M1p[l], M1l1, M1l2,
                          T1l1 = T1ml1, T1l2 = T1ml2,
                          T2l1 = T2ml1, T2l2 = T2ml2,
                          T3l1 = T3ml1,T3l2 = T3ml2,
                          jj = as.numeric((l-mean_j)/sd_j))
      T1mp[l] <- pFunc(fitR[[3]], dT1mp)

      # T2
      dT2mp <- data.frame(Vp, A = am, Al1 = am, Al2 = am,
                          M1 = M1p[l], M1l1, M1l2,
                          T1l1 = T1ml1, T1l2 = T1ml2,
                          T2l1 = T2ml1, T2l2 = T2ml2,
                          T3l1 = T3ml1, T3l2 = T3ml2,
                          jj = as.numeric((l-mean_j)/sd_j))
      T2mp[l] <- predict(fitR[[4]], dT2mp)

      # T3
      dT3mp <- data.frame(Vp, A = am, Al1 = am, Al2 = am,
                          M1 = M1p[l], M1l1, M1l2,
                          T1l1 = T1ml1, T1l2 = T1ml2,
                          T2l1 = T2ml1, T2l2 = T2ml2,
                          T3l1 = T3ml1, T3l2 = T3ml2,
                          jj = as.numeric((l-mean_j)/sd_j))
      T3mp[l] <- predict(fitR[[5]], dT3mp)


      # Predict time-varying covariates (contribute to outcome models, if ay != am)
      # If ay = am ==> simply covariates take the same values between two models

      if (ay != am) {
        # T1
        dT1p <- data.frame(Vp, A = ay, Al1 = ay, Al2 = ay,
                           M1 = M1p[l], M1l1, M1l2,
                           T1l1, T1l2,
                           T2l1, T2l2,
                           T3l1, T3l2,
                           jj = as.numeric((l-mean_j)/sd_j))
        T1p[l] <- pFunc(fitR[[3]], dT1p)

        # T2
        dT2p <- data.frame(Vp, A = ay, Al1 = ay, Al2 = ay,
                           M1 = M1p[l], M1l1, M1l2,
                           T1l1, T1l2,
                           T2l1, T2l2,
                           T3l1, T3l2,
                           jj = as.numeric((l-mean_j)/sd_j))
        T2p[l] <- predict(fitR[[4]], dT2p)

        # T3
        dT3p <- data.frame(Vp, A = ay, Al1 = ay, Al2 = ay,
                           M1 = M1p[l], M1l1, M1l2,
                           T1l1, T1l2,
                           T2l1, T2l2,
                           T3l1, T3l2,
                           jj = as.numeric((l-mean_j)/sd_j))
        T3p[l] <- predict(fitR[[5]], dT3p)



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
                        jj = as.numeric((l-mean_j)/sd_j))
      Yp[l] <- pFunc(fitR[[2]], dYp)

    } else {
      break
    }

    mm[l] <- l

  }


  boot_num <- seed
  gdat <- data.frame(boot_num, id, id_ori, mm, Ay = ay, Am = am, M1p, Yp,
                     T1mp, T1p, T2mp, T2p, T3mp, T3p, Vp)
  gdat$lastid <- as.numeric(!duplicated(gdat$id, fromLast = T))
  return(gdat)
}

library(tvmedg)
library(dplyr)

## seed = 123
df <- sim_data |>
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
  ) |> ungroup() %>% df_prep()

set.seed(123)

clusters <- names(table(df$id))
index <- sample(1:length(clusters), length(clusters), replace = TRUE)
bb <- table(clusters[index])

boot <- NULL

for(zzz in 1:max(bb)) {
  # Loop over repeated id
  cc <- df[df$id %in% names(bb[bb %in% c(zzz:max(bb))]), ]
  cc$bid <- paste0(cc$id, zzz)
  boot <- rbind(boot, cc)
}

boot$jj <- scale(boot$j)
mean_j <- attributes(boot$jj)$`scaled:center`
sd_j <- attributes(boot$jj)$`scaled:scale`
boot$jj <- as.numeric(boot$jj)

#----- fit parametric models for
#--- Mediator models

mM1 <- function(k){
  fitM1 <- glm(M1 ~ A + Al1 + Al2 + M1l1 + M1l2 + T1l1 + T1l2 + T2l1 + T2l2 + T3l1 + T3l2 +
                 v1 + v2 + v3 + v4 + splines::bs(jj, df = 3),
               family = binomial, data = boot)
  return(fitM1)
}

# PseudoR2(fitM1)
#--- Covariate models

mT1 <- function(k){
  fitT1 <- glm(T1 ~ A + Al1 + Al2 + M1 + M1l1 + M1l2 + T1l1 + T1l2 + T2l1 + T2l2 + T3l1 + T3l2 +
                 v1 + v2 + v3 + v4 + splines::bs(jj, df = 3),
               family = binomial, data = boot)
  return(fitT1)
}

# PseudoR2(fitT1)

mT2 <- function(k){
  fitT2 <- lm(T2 ~ A + Al1 + Al2 + M1 + M1l1 + M1l2 + T1l1 + T1l2 + T2l1 + T2l2 + T3l1 + T3l2 +
                v1 + v2 + v3 + v4 + splines::bs(jj, df = 3), data = boot)
  return(fitT2)
}

mT3 <- function(k){
  fitT3 <- lm(T3 ~ A + Al1 + Al2 + M1 + M1l1 + M1l2 + T1l1 + T1l2 + T2l1 + T2l2 + T3l1 + T3l2 +
                v1 + v2 + v3 + v4 + splines::bs(jj, df = 3), data = boot)
  return(fitT3)
}

# Outcome model: E(Y|a, m, l, v)

mY <- function(k) {
  fitY <- glm(Y ~ A + Al1 + Al2 + M1 + M1l1 + M1l2 + T1 + T1l1 + T1l2 + T2 + T2l1 + T2l2 +
                T3 + T3l1 + T3l2 +  v1 + v2 + v3 + v4 + splines::bs(jj, df = 3),
              family = binomial, data = boot)
  return(fitY)
}

# PseudoR2(fitY)

# Fit all models and save in a list
mR <- c(mM1, mY, mT1, mT2, mT3)
fitR <- lapply(1:5,function(x) mR[[x]](k))

montecarlo = 10000
# Select baseline visit
df0 <- boot[boot$j==1, ]

df0$idn <- 1:nrow(df0)

MC <- NULL

samples <- sample(df0$idn, size = montecarlo, replace = T)
bb <- table(samples)

for(zzz in 1:max(bb)) {
  cc <- df0[df0$idn %in% names(bb[bb %in% c(zzz:max(bb))]), ]
  cc$bid <- paste0(cc$idn, zzz)
  MC <- rbind(MC, cc)
}

MC$idsim <- 1:montecarlo



seed = 123
set.seed(seed)
along_redf <- gform(ii = 1, pgdat = MC, length = 12, ay = 1, am = 1) %>%
  as.data.frame()

set.seed(123)
test_redf <- process_data(
  fix = c("age","sex","ow","risk"),
  expo = c("Ap"),
  med = c("Mp"),
  tvar = c("L1","L2","L3"),
  outc = c("Yp"),
  lag = 2,
  norev = c("Mp"),
  time = c("mm"),
  data = sim_data
) %>% fitg(boot=T,
           mreg = "binomial",
           lreg = c("binomial","gaussian","gaussian"),
           yreg = "binomial",dof = 3) %>%
  baseline_mc(montecarlo = 10000) %>%
  g_form(ii = 1, length = 12, ay = 1, am = 1) %>% data.frame()

test_that("gform function", {

  # boot_num - Am
  expect_equal(along_redf[,c(2:6)],test_redf[,c(1:5)])
  expect_equal(along_redf[,c("v1","v2","v3","v4","lastid")],
               test_redf[,c("v1","v2","v3","v4","lastid")])

  expect_equal(along_redf[,c("M1p")],test_redf[,c("M1")])
  expect_equal(along_redf[,c("T1mp")],test_redf[,c("Lmp1")])
  expect_equal(along_redf[,c("T1p")],test_redf[,c("Lp1")])
  expect_equal(along_redf[,c("T2mp")],test_redf[,c("Lmp2")])
  expect_equal(along_redf[,c("T2p")],test_redf[,c("Lp2")])
  expect_equal(along_redf[,c("T3mp")],test_redf[,c("Lmp3")])
  expect_equal(along_redf[,c("T3p")], test_redf[,c("Lp3")])
  expect_equal(along_redf[,c("Yp")], test_redf[,c("Yp2")])
})
