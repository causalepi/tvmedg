library(tvmedg)
library(dplyr)
library(splines)

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

boot <- df

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


## package
fitR2 <- process_data(
  fix = c("age","sex","ow","risk"),
  expo = c("Ap"),
  med = c("Mp"),
  tvar = c("L1","L2","L3"),
  outc = c("Yp"),
  lag = 2,
  time = c("mm"),
  data = sim_data
) %>% fitg(boot=F,
           mreg = "binomial",
           lreg = c("binomial","gaussian","gaussian"),
           yreg = "binomial",dof = 3)

test_that("fitg function", {

  ## model M(t)
  expect_equal(min(fitR[[1]]$fitted.values),min(fitR2$M[[1]]$fitted.values))
  expect_equal(max(fitR[[1]]$fitted.values),max(fitR2$M[[1]]$fitted.values))
  expect_equal(mean(fitR[[1]]$fitted.values),mean(fitR2$M[[1]]$fitted.values))
  expect_equal(sd(fitR[[1]]$fitted.values),sd(fitR2$M[[1]]$fitted.values))

  ## model Y
  expect_equal(min(fitR[[2]]$fitted.values),min(fitR2$Y$fitted.values))
  expect_equal(max(fitR[[2]]$fitted.values),max(fitR2$Y$fitted.values))
  expect_equal(mean(fitR[[2]]$fitted.values),mean(fitR2$Y$fitted.values))
  expect_equal(sd(fitR[[2]]$fitted.values),sd(fitR2$Y$fitted.values))

  ## model L1
  expect_equal(min(fitR[[3]]$fitted.values),min(fitR2$L[[1]]$fitted.values))
  expect_equal(max(fitR[[3]]$fitted.values),max(fitR2$L[[1]]$fitted.values))
  expect_equal(mean(fitR[[3]]$fitted.values),mean(fitR2$L[[1]]$fitted.values))
  expect_equal(sd(fitR[[3]]$fitted.values),sd(fitR2$L[[1]]$fitted.values))

  ## model L2
  expect_equal(min(fitR[[4]]$fitted.values),min(fitR2$L[[2]]$fitted.values))
  expect_equal(max(fitR[[4]]$fitted.values),max(fitR2$L[[2]]$fitted.values))
  expect_equal(mean(fitR[[4]]$fitted.values),mean(fitR2$L[[2]]$fitted.values))
  expect_equal(sd(fitR[[4]]$fitted.values),sd(fitR2$L[[2]]$fitted.values))

  ## model L3
  expect_equal(min(fitR[[5]]$fitted.values),min(fitR2$L[[3]]$fitted.values))
  expect_equal(max(fitR[[5]]$fitted.values),max(fitR2$L[[3]]$fitted.values))
  expect_equal(mean(fitR[[5]]$fitted.values),mean(fitR2$L[[3]]$fitted.values))
  expect_equal(sd(fitR[[5]]$fitted.values),sd(fitR2$L[[3]]$fitted.values))
})
