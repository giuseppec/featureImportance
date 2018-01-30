featureImportanceLearner = function(learner, task, resampling, measures, weights = NULL,
  features = getTaskFeatureNames(task), n.feat.perm = 50, ...) {
  if (inherits(measures, "Measure"))
    measures = list(measures)
  mid = BBmisc::vcapply(measures, function(x) x$id)
  measures = setNames(measures, mid)
  # instantiate resampling
  if (inherits(resampling, "ResampleDesc"))
    resampling = mlr::makeResampleInstance(resampling, task = task)
  assertClass(resampling, classes = "ResampleInstance")

  # compute performance on unpermuted data
  data = mlr::getTaskData(task)
  res = mlr::resample(learner, task, resampling, measures, weights,
    models = TRUE, keep.pred = FALSE)

  # FIXME: using n.feat.perm instead of repeated CV should be faster as resampling is done only once
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
  x$importance$features = stri_paste_list(x$importance$features, sep = ",")
  if ("obs" %in% colnames(x$importance))
    by = c("obs", by)
  print(x$importance[, lapply(.SD, mean), .SDcols = measure.id, by = c("features", by)], ...)
}
