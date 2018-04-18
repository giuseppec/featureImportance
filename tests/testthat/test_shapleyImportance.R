context("shapleyImportance with WrappedModel works")
test_that("shapleyImportance with WrappedModel works", {
  feat = features

  set.seed(1)
  imp = shapleyImportance(mod, data = d, features = feat, n.feat.perm = n.feat.perm, measures = measures, local = FALSE)
  expect_output(print.ShapleyImportance(imp), "Shapley")

  # check if using mod$learner.model yields the same importances
  set.seed(1)
  imp2 = shapleyImportance(mod$learner.model, data = d, target = target, features = feat, n.feat.perm = n.feat.perm,
    measures = measures.fun, local = FALSE, predict.fun = predict.fun)
  expect_identical(imp$shapley.value, imp2$shapley.value)

  # FIXME: check if shapleyImportance local importance works
  # imp = shapleyImportance(mod, data = d, features = feat, n.feat.perm = n.feat.perm, n.shapley.perm = 1, measures = measures, local = TRUE)
})
