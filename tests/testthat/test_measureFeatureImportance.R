context("measureFeatureImportance works")

test_that("measureFeatureImportance works with WrappedModel and ResampleResult", {
  d2 = permuteFeature(d, features[1])
  mod.list = c(list(mod), res.list)

  for (m in mod.list) {
    # check if measure performance works with WrappedModel
    perf1 = measurePerformance(m, data = d, measures = measures, local = FALSE)
    perf2 = measurePerformance(m, data = d2, measures = measures, local = FALSE)

    imp12 = measureFeatureImportance(perf1, perf2)
    imp21 = measureFeatureImportance(perf2, perf1)
    expect_equal(imp12, -imp21)
    expect_true(all(measureFeatureImportance(perf1, perf1) == 0))

    perf1.local = measurePerformance(m, data = d, measures = measures, local = TRUE)
    perf2.local = measurePerformance(m, data = d2, measures = measures, local = TRUE)
    imp12.local = measureFeatureImportance(perf1.local, perf2.local)
    expect_data_table(imp12.local, ncols = ncol(perf1.local), nrows = nrow(perf1.local))
    expect_names(colnames(imp12.local), identical.to = colnames(perf1.local))

    if (inherits(m, "ResampleResult")) {
      expect_equal(as.numeric(perf1), unname(m$aggr))
      if (getRRPredictions(m)$instance$desc$id != "OOB bootstrapping")
        expect_equal(imp12, imp12.local[, -"row.id"][, lapply(.SD, mean)])
    }

    if (inherits(m, "WrappedModel")) {
      perf1.lrn = measurePerformance(m$learner.model, data = d, measures = measures.fun,
        target = target, predict.fun = predict.fun, local = FALSE)
      expect_equal(perf1, perf1.lrn)
      perf1.local.lrn = measurePerformance(m$learner.model, data = d, measures = measures.fun,
        local = TRUE, target = target, predict.fun = predict.fun)
      expect_equal(perf1.local, perf1.local.lrn)
    }
  }
})
