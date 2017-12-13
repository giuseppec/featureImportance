# helper functions
# @param object the trained model or resample result
# @param data the test data that should be shuffled and on which the performance drop is computed. You could also use the train data here.
# @param vector of features for which the importance is computed
performanceDrop = function(object, data, features, measures, n.feat.perm = 1) {
  UseMethod("performanceDrop")
}

performanceDrop.WrappedModel = function(object, data, features, measures, n.feat.perm = 1) {
  assertClass(object, "WrappedModel")
  assertDataFrame(data)
  #assertCharacter(features)
  #assertSubset(features, colnames(data))
  if (inherits(measures, "Measure"))
    measures = list(measures)
  assertList(measures, "Measure")

  p.true = predict(object, newdata = data)
  perf.true = mlr::performance(p.true, measures)

  minimize = !BBmisc::vlapply(measures, function(x) x$minimize)
  drop = lapply(features, function(feature) {
    # measure performance when feature is shuffled
    perf.shuffled = replicate(n.feat.perm, {
      measurePerformance(object, data, feature, measures, shuffle = TRUE)
      }, simplify = FALSE)
    perf.drop = lapply(perf.shuffled, function(p) measurePerformanceDrop(p, perf.true, measures, minimize))
    data.table::rbindlist(perf.drop, idcol = "n.feat.perm")
  })
  data.table::rbindlist(setNames(drop, features), idcol = "features")
}

# @param object resample result
# @param data the full data on which the resample was performed
performanceDrop.ResampleResult = function(object, data, features, measures, n.feat.perm = 1) {
  assertClass(object, "ResampleResult")
  if (is.null(object$models))
    stop("Use 'models = TRUE' to create the ResampleResult.")

  # for each fold and each feature: permute the feature and measure performance on permuted feature
  ret = lapply(object$models, function(mod) {
    train.ind = mod$subset
    performanceDrop.WrappedModel(object = mod, data = data[-train.ind,],
      features = features, measures = measures, n.feat.perm = n.feat.perm)
  })

  data.table::rbindlist(ret, idcol = "cv.iter")
}

# measures the drop in performance for a given (true) performance and the performance when a feature was shuffled
# @param perf.shuffled a vector of the performance(s) when a feature was shuffled
# @param perf.true a vector of the true performance(s)
# @param measures the performance measures that have been used: if big values for the measure are better, the drop in performance is true - permuted (negative "drop" values are performance "gains")
measurePerformanceDrop = function(perf.shuffled, perf.true, measures, minimize) {
  drop = perf.true - perf.shuffled
  sign = ifelse(minimize, 1, -1)
  return(as.data.frame(t(sign*drop)))
}

# shuffles feature in test data, predicts and measures performance
measurePerformance = function(mod, data, feature, measures, shuffle = TRUE) {
  #assertClass(mod, "WrappedModel")
  if (shuffle)
    data = permuteFeature(data, feature)
  p = predict(mod, newdata = data)
  perf = performance(p, measures)
  return(perf)
}
