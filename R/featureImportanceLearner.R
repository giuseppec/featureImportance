featureImportanceLearner = function(learner, task, resampling, measures, weights = NULL,
  features = as.list(getTaskFeatureNames(task)), n.feat.perm = 50, ...) {

  measures = assertMeasure(measures)

  # instantiate resampling
  if (inherits(resampling, "ResampleDesc"))
    resampling = mlr::makeResampleInstance(resampling, task = task)
  assertClass(resampling, classes = "ResampleInstance")

  # compute performance on unpermuted data
  data = mlr::getTaskData(task)
  res = mlr::resample(learner, task, resampling, measures, weights,
    models = TRUE)

  imp = featureImportance(object = res, data = data, features = features,
    n.feat.perm = n.feat.perm, measures = measures, ...)

  makeS3Obj(
    classes = "featureImportance",
    importance = imp,
    resample = res,
    measures = measures
  )
}

print.featureImportance = function(x, measure.id = names(x$measures), by = NULL, ...) {
  # expr = parse(text = sprintf("%s := mean(%s)", measure.id, measure.id))
  # return(x$importance[, eval(expr), by = "feature"])
  assertSubset(by, c("cv.iter", "n.feat.perm"), empty.ok = TRUE)
  catf("Object of class 'featureImportance'")
  catf("Aggregated importance:")
  if ("obs" %in% colnames(x$importance))
    by = c("obs", by)
  print(x$importance[, lapply(.SD, mean), .SDcols = measure.id, by = c("features", by)], ...)
}
