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
montecarlo= 100
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

##
set.seed(123)
fitR2 <- process_data(
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
) |> fitg(boot=T,
          mreg = "binomial",
          lreg = c("binomial","gaussian","gaussian"),
          yreg = "binomial") |>
  baseline_mc(montecarlo = 100) |> suppressWarnings()

### test base_line MC
along_redf <- rbindlist(MC_list2) |> as.data.frame()
test_redf <- rbindlist(fitR2$res_df) |> as.data.frame()

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
