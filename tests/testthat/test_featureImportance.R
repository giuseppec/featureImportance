context("featureImportance with WrappedModel works")
test_that("featureImportance with WrappedModel works", {
  n.feat.perm = 2
  set.seed(1)
  perf.drop = featureImportance(mod, data = d, features = as.list(features), n.feat.perm = n.feat.perm, measures = measures, local = FALSE)
  expect_data_table(perf.drop, nrows = length(features)*n.feat.perm)
  expect_set_equal(c("features", "n.feat.perm", mid), colnames(perf.drop))

  # check if featureImportance
  set.seed(1)
  measures.fun = list(acc = measureACC, mmce = measureMMCE)
  perf.drop2 = featureImportance(mod$learner.model, data = d, target = target, features = as.list(features), n.feat.perm = n.feat.perm,
    measures = measures.fun, minimize = c(acc = FALSE, mmce = TRUE), local = FALSE,
    predict.fun = function(object, newdata) predict(object, newdata, type = "class"))
  expect_data_table(perf.drop2, nrows = length(features)*n.feat.perm)
  expect_set_equal(c("features", "n.feat.perm", names(measures.fun)), colnames(perf.drop2))
})

context("featureImportance with ResampleResult works")
test_that("featureImportance with ResampleResult works", {
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

    perf.drop = featureImportance(res, data = d, features = feat, n.feat.perm = n.feat.perm, measures = measures, local = FALSE)
    expect_data_table(perf.drop, nrows = rin$desc$iters*length(feat))
    expect_set_equal(c("cv.iter", "features", "n.feat.perm", mid), colnames(perf.drop))

    perf.drop.local = featureImportance(res, data = d, features = feat, n.feat.perm = n.feat.perm, measures = measures, local = TRUE)
    n.test = length(unlist(rin$test.inds))
    expect_data_table(perf.drop.local, nrows = length(feat)*n.test)
    expect_set_equal(c("cv.iter", "features", "n.feat.perm", "obs", mid), colnames(perf.drop.local))
    expect_set_equal(res$pred$data$id, perf.drop.local$obs)
  }
})
