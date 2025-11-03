test_that("datasets load and have expected columns", {
  expect_true(all(c("date","year","month","hai","cases_month") %in% names(sim_monthly)))
  expect_true(all(c("date","year","iso_week","hai","cases_week") %in% names(sim_weekly)))
  expect_true(all(c("date","year","yday","wday","hai","cases_day") %in% names(sim_daily)))

  yrs_m <- unique(sim_monthly$year)
  yrs_w <- unique(sim_weekly$year)
  yrs_d <- unique(sim_daily$year)
  expect_true(all(yrs_m %in% 2011:2012))
  expect_true(all(yrs_w %in% 2011:2012))
  expect_true(all(yrs_d %in% 2011:2012))
})
