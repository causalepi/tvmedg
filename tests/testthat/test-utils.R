library(tvmedg)

dat <- process_data(
    fix = c("age","sex","ow","risk"),
    expo = c("Ap"),
    med = c("Mp"),
    tvar = c("L1","L2","L3"),
    outc = c("Yp"),
    lag = 2,
    time = c("mm"),
    data = sim_data
  )

boot <- dat$df
boot$jj <- scale(boot$j)
mean_j <- attributes(boot$jj)$`scaled:center`
sd_j <- attributes(boot$jj)$`scaled:scale`
boot$jj <- as.numeric(boot$jj)

boot2 <- cen_and_scale(dat$df$j)

test_that("cen and scale function", {
  expect_equal(boot$jj,boot2$jj)
  expect_equal(mean_j,boot2$mean_j)
  expect_equal(sd_j,boot2$sd_j)

})






