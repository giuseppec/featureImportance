test_that("measurePerformance and measurePerformanceDrop", {
  context("measurePerformance works")
  perf = measurePerformance(mod, data = d, features, measures, shuffle = FALSE, local = FALSE)
  expect_data_frame(perf, nrows = 1, ncols = length(measures))
  expect_set_equal(colnames(perf), mid)

  perf.local = measurePerformance(mod, data = d, features, measures, shuffle = FALSE, local = TRUE)
  expect_data_frame(perf.local, nrows = nrow(d), ncols = length(measures))
  expect_set_equal(colnames(perf.local), mid)

  context("measurePerformanceDrop works")
  # measurePerformanceDrop
  perf2 = measurePerformance(mod, data = d, features, measures, shuffle = TRUE, local = FALSE)
  perf.local2 = measurePerformance(mod, data = d, features, measures, shuffle = TRUE, local = TRUE)

  zero.drop = measurePerformanceDrop(perf.shuffled = perf, perf.true = perf, minimize = minimize)
  expect_data_frame(zero.drop, nrows = 1, ncols = length(measures))
  expect_true(all(zero.drop == 0))

  zero.drop.local = measurePerformanceDrop(perf.shuffled = perf.local, perf.true = perf.local, minimize = minimize)
  expect_data_frame(zero.drop.local, nrows = nrow(d), ncols = length(measures) + 1)
  expect_set_equal(colnames(zero.drop.local), c(mid, "obs"))
  expect_true(identical(zero.drop.local$obs, seq_row(d)))
  expect_true(all(zero.drop.local[, mid] == 0))
})
