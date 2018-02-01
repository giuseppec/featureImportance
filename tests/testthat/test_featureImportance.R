context("featureImportance with WrappedModel works")
test_that("featureImportance with WrappedModel works", {
  n.feat.perm = 2
  feat = as.list(features)
  # minimize = !BBmisc::vlapply(measures, function(x) x$minimize)

  set.seed(1)
  imp = featureImportance(mod, data = d, features = feat, n.feat.perm = n.feat.perm, measures = measures, local = FALSE)
  nrow = length(feat)*n.feat.perm
  expect_data_table(imp, nrows = nrow)
  expect_set_equal(colnames(imp), c("features", "n.feat.perm", mid))
  expect_equal(imp$acc, imp$mmce)
  expect_equal(stri_split_fixed(unique(imp$features), ","), feat)

  # check if using mod$learner.model yields the same importances
  measures.fun = list(acc = measureACC, mmce = measureMMCE)
  mid = names(measures.fun)
  minimize = c(acc = FALSE, mmce = TRUE)
  predict.fun = function(object, newdata) predict(object, newdata, type = "class")

  set.seed(1)
  imp2 = featureImportance(mod$learner.model, data = d, target = target, features = feat, n.feat.perm = n.feat.perm,
    measures = measures.fun, minimize = minimize, local = FALSE, predict.fun = predict.fun)
  expect_identical(imp, imp2)

  # check if featureImportance local importance works
  imp = featureImportance(mod, data = d, features = feat, n.feat.perm = n.feat.perm, measures = measures, local = TRUE)
  nrow = length(feat)*n.feat.perm*nrow(d)
  expect_data_table(imp, nrows = nrow)
  expect_set_equal(colnames(imp), c("features", "n.feat.perm", "obs", mid))
  expect_equal(imp$acc, imp$mmce)
  expect_equal(stri_split_fixed(unique(imp$features), ","), feat)
})

context("featureImportance with ResampleResult works")
test_that("featureImportance with ResampleResult works", {
  n.feat.perm = 2
  feat = list(features[1:2], features[3:4])

  resampling = list(
    makeResampleInstance(makeResampleDesc("CV", iters = 2), task),
    makeResampleInstance(makeResampleDesc("Bootstrap", iters = 2), task),
    makeResampleInstance(makeResampleDesc("RepCV", folds = 2, reps = 2), task)
  )

  for (rin in resampling) {
    res = mlr::resample(learner, task, rin, measures, models = TRUE, keep.pred = TRUE)
    d = getTaskData(task)

    imp = featureImportance(res, data = d, features = feat, n.feat.perm = n.feat.perm, measures = measures, local = FALSE)
    nrow = rin$desc$iters*length(feat)*n.feat.perm

    expect_data_table(imp, nrows = nrow)
    expect_set_equal(c("cv.iter", "features", "n.feat.perm", mid), colnames(imp))

    imp.local = featureImportance(res, data = d, features = feat, n.feat.perm = n.feat.perm, measures = measures, local = TRUE)
    nrow = length(feat)*length(unlist(rin$test.inds))*n.feat.perm

    expect_data_table(imp.local, nrows = nrow)
    expect_equal(imp.local$acc, imp.local$mmce)
    expect_equal(stri_split_fixed(unique(imp.local$features), ","), feat)
    expect_set_equal(colnames(imp.local), c("cv.iter", "features", "n.feat.perm", "obs", mid))
    expect_set_equal(res$pred$data$id, imp.local$obs)
  }
})
