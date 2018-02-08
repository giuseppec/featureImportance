context("featureImportance with WrappedModel works")
test_that("featureImportance with WrappedModel works", {
  feat = as.list(features)

  set.seed(1)
  imp = featureImportance(mod, data = d, features = feat, n.feat.perm = n.feat.perm, measures = measures, local = FALSE)
  imp = imp$importance
  nrow = length(feat)*n.feat.perm
  expect_data_table(imp, nrows = nrow)
  expect_set_equal(colnames(imp), c("features", "n.feat.perm", mid))
  expect_equal(imp$acc, imp$mmce)
  expect_equal(stri_split_fixed(unique(imp$features), ","), feat)

  # check if using mod$learner.model yields the same importances
  set.seed(1)
  predict.fun = function(object, newdata) predict(object, newdata, type = "class")
  imp2 = featureImportance(mod$learner.model, data = d, target = target, features = feat, n.feat.perm = n.feat.perm,
    measures = measures.fun, minimize = minimize, local = FALSE, predict.fun = predict.fun)
  imp2 = imp2$importance
  expect_identical(imp, imp2)

  # check if featureImportance local importance works
  imp = featureImportance(mod, data = d, features = feat, n.feat.perm = n.feat.perm, measures = measures, local = TRUE)
  imp = imp$importance
  nrow = length(feat)*n.feat.perm*nrow(d)
  expect_data_table(imp, nrows = nrow)
  expect_set_equal(colnames(imp), c("features", "n.feat.perm", "obs", mid))
  expect_equal(imp$acc, imp$mmce)
  expect_equal(stri_split_fixed(unique(imp$features), ","), feat)
})

context("featureImportance with ResampleResult works")
test_that("featureImportance with ResampleResult works", {
  feat = list(features[1:2], features[3:4])

  for (i in seq_along(res.list)) {
    res = res.list[[i]]
    rin = resampling[[i]]
    imp = featureImportance(res, data = d, features = feat, n.feat.perm = n.feat.perm, measures = measures, local = FALSE)
    imp = imp$importance
    nrow = rin$desc$iters*length(feat)*n.feat.perm

    expect_data_table(imp, nrows = nrow)
    expect_set_equal(c("cv.iter", "features", "n.feat.perm", mid), colnames(imp))

    imp.local = featureImportance(res, data = d, features = feat, n.feat.perm = n.feat.perm, measures = measures, local = TRUE)
    imp.local = imp.local$importance
    nrow = length(feat)*length(unlist(rin$test.inds))*n.feat.perm

    expect_data_table(imp.local, nrows = nrow)
    expect_equal(imp.local$acc, imp.local$mmce)
    expect_equal(stri_split_fixed(unique(imp.local$features), ","), feat)
    expect_set_equal(colnames(imp.local), c("cv.iter", "features", "n.feat.perm", "obs", mid))
    expect_set_equal(res$pred$data$id, imp.local$obs)
  }
})
