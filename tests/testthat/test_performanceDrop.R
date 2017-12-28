context("performanceDrop with WrappedModel works")
test_that("performanceDrop with WrappedModel works", {
  n.feat.perm = 2
  perf.drop = performanceDrop(mod, data = d, features = features, n.feat.perm = n.feat.perm, measures = measures, local = FALSE)
  expect_data_table(perf.drop, nrows = length(features)*n.feat.perm)
  expect_set_equal(c("features", "n.feat.perm", mid), colnames(perf.drop))
})


context("performanceDrop with ResampleResult works")
test_that("performanceDrop with ResampleResult works", {
  n.feat.perm = 1
  feat = features[1]

  resampling = list(
    makeResampleInstance(makeResampleDesc("CV", iters = 2), task),
    makeResampleInstance(makeResampleDesc("Bootstrap", iters = 2), task),
    makeResampleInstance(makeResampleDesc("RepCV", folds = 2, reps = 2), task)
  )

  for (rin in resampling) {
    res = mlr::resample(learner, task, rin, measures, models = TRUE, keep.pred = TRUE)
    d = getTaskData(task)

    perf.drop = performanceDrop(res, data = d, features = feat, n.feat.perm = n.feat.perm, measures = measures, local = FALSE)
    expect_data_table(perf.drop, nrows = rin$desc$iters*length(feat))
    expect_set_equal(c("cv.iter", "features", "n.feat.perm", mid), colnames(perf.drop))

    perf.drop.local = performanceDrop(res, data = d, features = feat, n.feat.perm = n.feat.perm, measures = measures, local = TRUE)
    n.test = length(unlist(rin$test.inds))
    expect_data_table(perf.drop.local, nrows = length(feat)*n.test)
    expect_set_equal(c("cv.iter", "features", "n.feat.perm", "obs", mid), colnames(perf.drop.local))
    expect_true(identical(res$pred$data$id, perf.drop.local$obs))
  }
})
