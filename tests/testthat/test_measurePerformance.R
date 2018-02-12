context("measurePerformance works")
test_that("measurePerformance works", {
  # check if measure performance works with WrappedModel
  perf = measurePerformance(mod, data = d, measures = measures, local = FALSE)
  expect_data_frame(perf, nrows = 1, ncols = length(measures))
  expect_set_equal(colnames(perf), mid)

  perf.local = measurePerformance(mod, data = d, measures = measures, local = TRUE)
  expect_data_frame(perf.local, nrows = nrow(d), ncols = length(measures) + 1)
  expect_set_equal(colnames(perf.local), c("row.id", mid))

  perf.local = measurePerformance(mod, data = d, measures = measures,
    local = TRUE, row.id = seq_row(d) + 10)
  expect_equal(perf.local$row.id, seq_row(d) + 10)
  expect_data_frame(perf.local, nrows = nrow(d), ncols = length(measures) + 1)
  expect_set_equal(colnames(perf.local), c("row.id", mid))

  # check if measure performance works with non-WrappedModel
  perf = measurePerformance(mod$learner.model, data = d, target = target,
    measures = measures.fun, local = FALSE,
    predict.fun = function(object, newdata) predict(object, newdata, type = "class"))
  expect_data_frame(perf, nrows = 1, ncols = length(measures.fun))
  expect_set_equal(colnames(perf), names(measures.fun))

  perf.local = measurePerformance(mod$learner.model, data = d, target = target,
    measures = measures.fun, local = TRUE,
    predict.fun = function(object, newdata) predict(object, newdata, type = "class"))
  expect_data_frame(perf.local, nrows = nrow(d), ncols = length(measures.fun) + 1)
  expect_set_equal(colnames(perf.local), c("row.id", names(measures.fun)))
})
