context("featureImportance with WrappedModel works")
test_that("featureImportance with WrappedModel works", {
  feat = as.list(features[1])
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
      plotImportance(imp, feat = feat[[1]], mid = "mmce", individual = FALSE)
    }
  }
})
