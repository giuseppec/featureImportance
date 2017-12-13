featureImportance = function(learner, task, resampling, measures, weights = NULL,
  features = getTaskFeatureNames(task), n.feat.perm = 1, ...) {
  # instantiate resampling
  if (inherits(resampling, "ResampleDesc"))
    resampling = mlr::makeResampleInstance(resampling, task = task)
  if (inherits(measures, "Measure"))
    measures = list(measures)
  mid = BBmisc::vcapply(measures, function(x) x$id)
  measures = setNames(measures, mid)
  assertClass(resampling, classes = "ResampleInstance")

  # compute performance on unpermuted data
  data = mlr::getTaskData(task)
  res = mlr::resample(learner, task, resampling, measures, weights,
    models = TRUE, keep.pred = FALSE)
  #perf.true = res$measures.test

  # FIXME: using n.feat.perm instead of repeated CV should be faster as resampling is done only once
  drop = performanceDrop(object = res, data = data, features = features,
    measures = measures, n.feat.perm = n.feat.perm)

  makeS3Obj(
    classes = "featureImportance",
    importance = drop,
    resample = res,
    measures = measures
  )
}

print.featureImportance = function(x, measure.id = names(x$measures)[1], ...) {
  # expr = parse(text = sprintf("%s := mean(%s)", measure.id, measure.id))
  # return(x$importance[, eval(expr), by = "feature"])
  print(x$importance)
  #x[, mean(acc), by = c("feature")]
}
