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

#' @export
print.featureImportance = function(x, measure.id = names(x$measures), digits = 2, ...) {
  # expr = parse(text = sprintf("%s := mean(%s)", measure.id, measure.id))
  # return(x$importance[, eval(expr), by = "feature"])
  catf("Object of class 'featureImportance'")
  catf(" Method for feature values:          %s", x$method)
  catf(" Importance measure based on:        %s", collapse(names(x$measures)))
  catf(" Contains local feature importance:  %s", ifelse(x$local, "yes", "no"))
  catf(" Global feature importance: ")
  res = summary(x, measure.id = names(x$measures))
  #res = transpose(mat[, -"features"])
  #colnames(res) = mat[["features"]]
  print(res, digits = digits, ...)
}

#' @export
summary.featureImportance = function(object, measure.id = names(object$measures), local = FALSE, ...) {
  if (object$local) {
    if ("feature.value" %in% colnames(object$importance))
      by = "feature.value" else
        by = NULL
    res = object$importance[, lapply(.SD, mean), .SDcols = measure.id, by = c("features", "row.id", by)]
  } else {
    res = object$importance[, lapply(.SD, mean), .SDcols = measure.id, by = c("features")]
  }
  if (!local)
    res = res[,  lapply(.SD, mean), .SDcols = measure.id, by = c("features")]

  return(res[order(get(measure.id), decreasing = TRUE)])
}
