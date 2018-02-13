#' @inherit featureImportance
#' @param learner [\code{\link[mlr]{Learner}}] \cr
#' The learner.
#' @inheritParams mlr::resample
#' @export
featureImportanceLearner = function(learner, task, resampling,
  measures = mlr::getDefaultMeasure(task),
  features = as.list(mlr::getTaskFeatureNames(task)), n.feat.perm = 50, ...) {

  assertClass(learner, "Learner")
  assertClass(task, "Task")
  # instantiate resampling
  if (inherits(resampling, "ResampleDesc"))
    resampling = mlr::makeResampleInstance(resampling, task = task)
  assertClass(resampling, "ResampleInstance")
  measures = assertMeasure(measures)

  # compute performance on unpermuted data
  data = mlr::getTaskData(task)
  res = mlr::resample(learner, task, resampling, measures, models = TRUE)

  imp = featureImportance(object = res, data = data, features = features,
    n.feat.perm = n.feat.perm, measures = measures, ...)

  makeS3Obj(
    classes = "featureImportance",
    importance = imp$importance,
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
  if ("row.id" %in% colnames(x$importance))
    by = c("row.id", by)
  print(x$importance[, lapply(.SD, mean), .SDcols = measure.id, by = c("features", by)], ...)
}
