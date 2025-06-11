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

##
set.seed(123)
fitR2 <- process_data(
  fix = c("age","sex","ow","risk"),
  expo = c("Ap"),
  med = c("Mp"),
  tvar = c("L1","L2","L3"),
  outc = c("Yp"),
  lag = 2,
  time = c("mm"),
  data = sim_data
) %>% fitg(boot=T,
           mreg = "binomial",
           lreg = c("binomial","gaussian","gaussian"),
           yreg = "binomial",dof = 3) %>%
  baseline_mc(montecarlo = 10000)

###
along_redf <- MC %>% as.data.frame()
test_redf <- fitR2$res_df %>% as.data.frame()

test_that("baseline_mc function", {

  # id, v1 - v4
  expect_equal(along_redf[,1],test_redf[,1])
  expect_equal(along_redf[,2],test_redf[,2])
  expect_equal(along_redf[,3],test_redf[,3])
  expect_equal(along_redf[,4],test_redf[,4])
  expect_equal(along_redf[,5],test_redf[,5])

  # exposure variables
  expect_equal(along_redf[,c("A")],test_redf[,c("A1")])
  expect_equal(along_redf[,c("Al1")],test_redf[,c("A1l1")])
  expect_equal(along_redf[,c("Al2")],test_redf[,c("A1l2")])

  ## time-dependent covariate
  expect_equal(along_redf[,c("T1")],test_redf[,c("L1")])
  expect_equal(along_redf[,c("T2")],test_redf[,c("L2")])
  expect_equal(along_redf[,c("T3")],test_redf[,c("L3")])

  ## time-dependent covariate + lag
  expect_equal(along_redf[,c("T1l1")],test_redf[,c("L1l1")])
  expect_equal(along_redf[,c("T2l1")],test_redf[,c("L2l1")])
  expect_equal(along_redf[,c("T3l1")],test_redf[,c("L3l1")])

  expect_equal(along_redf[,c("T1l2")],test_redf[,c("L1l2")])
  expect_equal(along_redf[,c("T2l2")],test_redf[,c("L2l2")])
  expect_equal(along_redf[,c("T3l2")],test_redf[,c("L3l2")])

  ## Mediator
  expect_equal(along_redf[,c("M1")],test_redf[,c("M1")])
  expect_equal(along_redf[,c("M1l1")],test_redf[,c("M1l1")])
  expect_equal(along_redf[,c("M1l2")],test_redf[,c("M1l2")])

  ## outcome and time
  expect_equal(along_redf[,c("Y")],test_redf[,c("Y")])
  expect_equal(along_redf[,c("j")],test_redf[,c("j")])
})
