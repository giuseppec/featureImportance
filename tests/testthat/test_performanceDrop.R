context("performanceDrop with WrappedModel works")
test_that("performanceDrop with WrappedModel works", {
  perf.drop = performanceDrop(mod, data = d, features = features, n.feat.perm = 1, measures = measures, local = FALSE)
  expect_data_table(perf.drop, nrows = length(features))
  expect_set_equal(c("features", "n.feat.perm", mid), colnames(perf.drop))
})


context("performanceDrop with ResampleResult works")
test_that("performanceDrop with ResampleResult works", {
  resampling = makeResampleInstance(makeResampleDesc("CV", iter = 3), task)
  res = mlr::resample(learner, task, resampling, measures, models = TRUE, keep.pred = FALSE)
  d = getTaskData(task)

  perf.drop = performanceDrop(res, data = d, features = features, n.feat.perm = 1, measures = measures, local = FALSE)
  expect_data_table(perf.drop)
  expect_set_equal(c("cv.iter", "features", "n.feat.perm", mid), colnames(perf.drop))
})
