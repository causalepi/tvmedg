library(dplyr)
library(tvmedg)
library(splines)

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
  ) |> ungroup() %>% df_prep()

## package
data_pro <- process_data(
  id = "id",
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
)

test_that("process_data", {
  ## id, v1 - v4
  expect_equal(dat[,c(1:5)],data_pro$df[,c(1:5)])

  ## exposure variables
  expect_equal(dat[,c("A")],data_pro$df[,c("A1")])
  expect_equal(dat[,c("Al1")],data_pro$df[,c("A1l1")])
  expect_equal(dat[,c("Al2")],data_pro$df[,c("A1l2")])

  ## time-dependent covariate
  expect_equal(dat[,c("T1")],data_pro$df[,c("L1")])
  expect_equal(dat[,c("T2")],data_pro$df[,c("L2")])
  expect_equal(dat[,c("T3")],data_pro$df[,c("L3")])

  ## time-dependent covariate + lag
  expect_equal(dat[,c("T1l1")],data_pro$df[,c("L1l1")])
  expect_equal(dat[,c("T2l1")],data_pro$df[,c("L2l1")])
  expect_equal(dat[,c("T3l1")],data_pro$df[,c("L3l1")])

  expect_equal(dat[,c("T1l2")],data_pro$df[,c("L1l2")])
  expect_equal(dat[,c("T2l2")],data_pro$df[,c("L2l2")])
  expect_equal(dat[,c("T3l2")],data_pro$df[,c("L3l2")])

  ## Mediator
  expect_equal(dat[,c("M1")],data_pro$df[,c("M1")])
  expect_equal(dat[,c("M1l1")],data_pro$df[,c("M1l1")])
  expect_equal(dat[,c("M1l2")],data_pro$df[,c("M1l2")])

  ## outcome and time
  expect_equal(dat[,c("Y","j")],data_pro$df[,c("Y","j")])
})

