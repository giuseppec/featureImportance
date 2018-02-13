#' @title Permutation Importance
#' @description Implementation of PIMP (see Altmann et al. (2010))
#' @references Altmann, A., Tolosi, L., Sander, O., & Lengauer, T. (2010).
#' Permutation importance: a corrected feature importance measure.
#' Bioinformatics, 26(10), 1340-1347.
#' @inheritParams featureImportanceLearner
#' @param n.target.perm [\code{numeric(1)}] \cr
#' The number of permutations of the target used to compute a null distribution for the importance values.
#' @export
pimp = function(learner, task, resampling, measures, features = NULL,
  n.feat.perm = 1, n.target.perm = 1) {
  measures = assertMeasure(measures)
  mid = names(measures)
  if (is.null(features))
    features = as.list(getTaskFeatureNames(task))

  # compute importance
  imp = featureImportanceLearner(learner, task, resampling, measures,
    features = features, n.feat.perm = n.feat.perm)
  imp.aggr = imp$importance[, lapply(.SD, mean), .SDcols = mid, by = c("features")]

  # compute null distribution when permuting the target 'n.target.perm' times
  null = nullImportance(learner, task, resampling, measures, features = features,
    n.feat.perm = n.feat.perm, n.target.perm = n.target.perm)
  null.aggr = null[, lapply(.SD, mean), .SDcols = mid, by = c("features", "n.target.perm")]

  # split by features and compute the pvalue separately for each feature
  sp = split(null.aggr[, mid, width = FALSE], null.aggr$features)
  ret = lapply(imp.aggr$features, function(x) {
    importance = imp.aggr[imp.aggr$feature == x, mid, width = FALSE]
    null.dist = sp[[x]]
    p.value = vnapply(mid, function(i) mean(null.dist[[i]] > importance[[i]]))
    p.value = as.data.table(t(p.value))
    list(importance = importance, p.value = p.value, null.dist = null.dist)
  })
  ret = setNames(ret, imp.aggr$features)

  importance = rbindlist(lapply(ret, function(x) x$importance), idcol = "features")
  p.value = rbindlist(lapply(ret, function(x) x$p.value), idcol = "features")
  null.dist = rbindlist(lapply(ret, function(x) x$null.dist), idcol = "features")

  makeS3Obj("pimp",
    importance = importance,
    p.value = p.value,
    null.dist = null.dist,
    raw = list(importance = importance, null = null)
  )
}

print.pimp = function(x) {
  catf("Object of class 'pimp'")
  catf("Feature importance with p-values:")
  print(merge(setkey(x$importance, "features"), setkey(x$p.value, "features"),
    suffixes = c(".importance", ".pvalue")))
}

# helper functions
nullImportance = function(learner, task, resampling, measures, features = NULL,
  n.feat.perm = 1, n.target.perm = 1) {
  if (is.null(features))
    features = as.list(getTaskFeatureNames(task))
  args = list(learner = learner, task = task, resampling = resampling,
    measures = measures, features = features, n.feat.perm = n.feat.perm)
  #parallelLibrary("featureImportance", master = FALSE, level = "n.target.perm", show.info = FALSE)
  ret = parallelMap::parallelMap(nullImportanceIteration, i = seq_len(n.target.perm),
    more.args = args)
  rbindlist(ret, idcol = "n.target.perm")
}

nullImportanceIteration = function(i, learner, task, resampling, measures, features, n.feat.perm) {
  d = mlr::getTaskData(task)
  target = mlr::getTaskTargetNames(task)
  # FIXME: maybe do not use unexported changeData here, can be problematic on CRAN
  task.null = changeTask(task, data = permuteFeature(d, target))
  imp = featureImportanceLearner(learner = learner, task.null, resampling = resampling,
    measures = measures, features = features, n.feat.perm = n.feat.perm)
  return(imp$importance)
}

# modified version of unexported mlr:::changeData
changeTask = function(task, data, costs, weights) {
  if (missing(data))
    data = getTaskData(task)
  if (missing(costs))
    costs = getTaskCosts(task)
  if (missing(weights))
    weights = task$weights
  task$env = new.env(parent = emptyenv())
  task$env$data = data
  if (is.null(weights))
    task["weights"] = list(NULL)
  else task$weights = weights
  return(task)
}
