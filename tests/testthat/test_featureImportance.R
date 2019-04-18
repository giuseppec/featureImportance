context("featureImportance with WrappedModel works")
test_that("featureImportance with WrappedModel works", {
  feat = list(features[1], features[2:3])
  local = c(FALSE, TRUE)
  method = c("permute", "replace.id")

  for (m in method) {
    for (loc in local) {
      set.seed(1)
      if (m == "permute") {
        imp = featureImportance(mod, data = d, features = feat, n.feat.perm = n.feat.perm, measures = measures, local = loc)
      } else {
        imp = featureImportance(mod, data = d, features = feat, replace.ids = 1:2, measures = measures, local = loc)
      }
      imp = imp$importance
      if (loc)
        expect_subset(c("row.id", "replace.id"), colnames(imp))
      nrow = length(feat)*n.feat.perm*ifelse(loc, nrow(d), 1)

      expect_output(print.featureImportance(imp), regexp = "Global feature importance")
      expect_data_table(imp, nrows = nrow)
      expect_subset(c("features", "n.feat.perm", mid), colnames(imp))
      expect_equal(imp$acc, -imp$mmce)
      expect_equal(stri_split_fixed(unique(imp$features), ","), feat)

      # check if using mod$learner.model yields the same importances
      set.seed(1)
      if (m == "permute") {
        imp2 = featureImportance(mod$learner.model, data = d, target = target, features = feat, n.feat.perm = n.feat.perm,
          measures = measures.fun, local = loc, predict.fun = predict.fun)
      } else {
        imp2 = featureImportance(mod$learner.model, data = d, target = target, features = feat, replace.ids = 1:2,
          measures = measures.fun, local = loc, predict.fun = predict.fun)
      }
      imp2 = imp2$importance
      expect_identical(imp, imp2)
    }
  }
})

context("featureImportance with ResampleResult works")
test_that("featureImportance with ResampleResult works", {
  feat = list(features[1:2], features[3])

  for (i in seq_along(res.list)) {
    res = res.list[[i]]
    rin = resampling[[i]]
    imp = featureImportance(res, data = d, features = feat, n.feat.perm = n.feat.perm, measures = measures, local = FALSE)
    imp = imp$importance
    nrow = length(feat)*n.feat.perm

    expect_data_table(imp, nrows = nrow)
    expect_subset(c("features", "n.feat.perm", mid), colnames(imp))
    expect_error(expect_warning(featureImportance(res, data = d[1:2,], features = feat,
      n.feat.perm = n.feat.perm, measures = measures, local = FALSE),
      regexp = "Use the same data that created the ResampleResult"))

    imp.local = featureImportance(res, data = d, features = feat, n.feat.perm = n.feat.perm, measures = measures, local = TRUE)
    imp.local = imp.local$importance
    nrow = length(feat)*length(unique(unlist(rin$test.inds)))*n.feat.perm

    expect_data_table(imp.local, nrows = nrow)
    expect_equal(imp.local$acc, -imp.local$mmce)
    expect_equal(stri_split_fixed(unique(imp.local$features), ","), feat)
    expect_subset(c("features", "n.feat.perm", "row.id", mid), colnames(imp.local)) # for CV there must be a replace.id column
    expect_set_equal(res$pred$data$id, imp.local$row.id)
  }
})


# context("featureImportance parallelization")
# test_that("featureImportance parallelization", {
#   feat = list(features[1:2], features[3])
#
#   plan(sequential)
#   for (i in seq_along(res.list)) {
#     res = res.list[[i]]
#     rin = resampling[[i]]
#     start_time = Sys.time()
#     imp = featureImportance(res, data = d, features = feat, n.feat.perm = 100L, measures = measures, local = FALSE)
#     end_time = Sys.time()
#     print(end_time - start_time)
#   }
#
#   library(future.apply)
#   plan(multiprocess(workers = 4L))
#   for (i in seq_along(res.list)) {
#     res = res.list[[i]]
#     rin = resampling[[i]]
#     start_time = Sys.time()
#     imp = featureImportance(res, data = d, features = feat, n.feat.perm = 100L, measures = measures, local = FALSE)
#     end_time = Sys.time()
#     print(end_time - start_time)
#   }
# })
