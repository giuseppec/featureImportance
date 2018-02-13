context("valueFunctions checks")
test_that("valueFunctions checks", {
  imp = calculateValueFunctionImportance(features, mod, d, target = NULL,
    n.feat.perm = n.feat.perm, measures, minimize = NULL, predict.fun = NULL)
  expect_data_table(imp, nrows = 1, ncols = length(measures) + 1)
  imp = calculateValueFunctionPerformance(features, mod, d, target = NULL,
    n.feat.perm = n.feat.perm, measures, minimize = NULL, predict.fun = NULL)
  expect_data_table(imp, nrows = 1, ncols = length(measures) + 1)

  imp = calculateValueFunctionImportance(features, mod$learner.model, d, target = target,
    n.feat.perm = n.feat.perm, measures.fun, minimize = minimize, predict.fun = predict.fun)
  expect_data_table(imp, nrows = 1, ncols = length(measures.fun) + 1)
  imp = calculateValueFunctionPerformance(features, mod$learner.model, d, target = target,
    n.feat.perm = n.feat.perm, measures.fun, minimize = minimize, predict.fun = predict.fun)
  expect_data_table(imp, nrows = 1, ncols = length(measures.fun) + 1)

  for (m in res.list) {
    imp = calculateValueFunctionImportance(features, m, d, n.feat.perm = n.feat.perm, measures = measures)
    expect_data_table(imp, nrows = 1, ncols = length(measures) + 1)
    imp = calculateValueFunctionPerformance(features, m, d, n.feat.perm = n.feat.perm, measures = measures)
    expect_data_table(imp, nrows = 1, ncols = length(measures) + 1)
  }
})


