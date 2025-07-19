library(tvmedg)
library(dplyr)
library(splines)
library(data.table)

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
seed = 123

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

fitT2 <- lm(T2 ~ A + Al1 + Al2 + M1 + M1l1 + M1l2 + T1l1 + T1l2 + T2l1 + T2l2 + T3l1 + T3l2 +
              v1 + v2 + v3 + v4 + bs(j, df = 3), data = boot) |> suppressWarnings()

fitT3 <- lm(T3 ~ A + Al1 + Al2 + M1 + M1l1 + M1l2 + T1l1 + T1l2 + T2l1 + T2l2 + T3l1 + T3l2 +
              v1 + v2 + v3 + v4 + bs(j, df = 3), data = boot) |> suppressWarnings()


# Outcome model: E(Y|a, m, l, v)

fitY <- glm(Y ~ A + Al1 + Al2 + M1 + M1l1 + M1l2 + T1 + T1l1 + T1l2 + T2 + T2l1 + T2l2 +
              T3 + T3l1 + T3l2 +  v1 + v2 + v3 + v4 + bs(j, df = 3),
            family = binomial, data = boot) |> suppressWarnings()

fitR <- list(fitM1, fitY, fitT1, fitT2, fitT3)


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
) |> fitg(boot=T,
          mreg = "binomial",
          lreg = c("binomial","gaussian","gaussian"),
          yreg = "binomial") |> suppressWarnings()

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
