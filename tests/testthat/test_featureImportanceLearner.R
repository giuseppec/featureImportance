context("featureImportanceLearner works")
test_that("featureImportanceLearner works", {
  n.feat.perm = 2
  feat = list(features[1:2], features[3:4])
  measures = list(acc, mmce)
  mid = BBmisc::vcapply(measures, function(x) x$id)

  resampling = list(
    makeResampleInstance(makeResampleDesc("CV", iters = 2), task),
    makeResampleInstance(makeResampleDesc("Bootstrap", iters = 2), task),
    makeResampleInstance(makeResampleDesc("RepCV", folds = 2, reps = 2), task)
  )

  # rin = resampling[[1]]

  for (rin in resampling) {
    imp = featureImportanceLearner(learner, task, rin, features = feat, n.feat.perm = n.feat.perm, measures = measures, local = FALSE)
    nrow = rin$desc$iters*length(feat)*n.feat.perm

    expect_data_table(imp$importance, nrows = nrow)
    expect_set_equal(c("cv.iter", "features", "n.feat.perm", mid), colnames(imp$importance))

    imp.local = featureImportanceLearner(learner, task, rin, features = feat, n.feat.perm = n.feat.perm, measures = measures, local = TRUE)
    # imp.local2 = lapply(feat, function(f) {
    #   featureImportanceLearner(learner, task, rin, features = list(f), n.feat.perm = n.feat.perm, measures = measures, local = TRUE)
    # })

    nrow = length(feat)*length(unlist(rin$test.inds))*n.feat.perm

    expect_data_table(imp.local, nrows = nrow)
    expect_equal(imp.local$acc, imp.local$mmce)
    expect_equal(unique(imp.local$features), feat)
    expect_set_equal(colnames(imp.local), c("cv.iter", "features", "n.feat.perm", "obs", mid))
    expect_set_equal(res$pred$data$id, imp.local$obs)
  }
})
