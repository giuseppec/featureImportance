pimp = function(learner, task, resampling, measures, features = NULL,
  n.feat.perm = 1, n.target.perm = 1) {
  measures = assertMeasure(measures)
  mid = names(measures)
  if (is.null(features))
    features = as.list(getTaskFeatureNames(task))

  imp = featureImportanceLearner(learner, task, resampling, measures, features = features, n.feat.perm = n.feat.perm)
  imp.aggr = imp$importance[, lapply(.SD, mean), .SDcols = mid, by = c("features")]

  null = nullImportance(learner, task, resampling, measures, features = features, n.feat.perm = n.feat.perm, n.target.perm = n.target.perm)
  null.aggr = null[, lapply(.SD, mean), .SDcols = mid, by = c("features", "n.target.perm")]

  # ret = merge(setkey(null.aggr, features), setkey(imp.aggr, features), suffixes = c(".null", ".imp"))

  sp = split(null.aggr[, ..mid], null.aggr$features)

  ret = setNames(lapply(imp.aggr$features, function(x) {
    importance = imp.aggr[imp.aggr$feature == x, ..mid]
    null.dist = sp[[x]]
    p.value = vnapply(mid, function(i) mean(null.dist[[i]] > importance[[i]]))

    #rbindlist(lapply(ret, function(x) x$importance), idcol = "features")

    list(importance = importance, p.value = as.data.table(t(p.value)), null.dist = null.dist)
  }), imp.aggr$features)

  importance = rbindlist(lapply(ret, function(x) x$importance), idcol = "features")
  p.value = rbindlist(lapply(ret, function(x) x$p.value), idcol = "features")
  null.dist = rbindlist(lapply(ret, function(x) x$null.dist), idcol = "features")

  makeS3Obj("PIMP",
    importance = importance,
    p.value = p.value,
    null.dist = null.dist,
    raw = list(importance = importance, null = null)
  )
}

print.PIMP = function(x) {
  catf("Object of class 'PIMP'")
  catf("Importance with PIMP p-values:")
  print(merge(setkey(x$importance, features), setkey(x$p.value, features), suffixes = c(".importance", ".pvalue")))
}

# helper functions
nullImportanceIteration = function(i, learner, task, resampling, measures, features, n.feat.perm) {
  d = mlr::getTaskData(task)
  target = mlr::getTaskTargetNames(task)
  # FIXME: maybe do not use unexported changeData here, can be problematic on CRAN
  task.null = mlr:::changeData(task, data = permuteFeature(d, target))
  imp = featureImportanceLearner(learner = learner, task.null, resampling = resampling,
    measures = measures, features = features, n.feat.perm = n.feat.perm)
  return(imp$importance)
}

nullImportance = function(learner, task, resampling, measures, features = NULL, n.feat.perm = 1, n.target.perm = 1) {
  if (is.null(features))
    features = as.list(getTaskFeatureNames(task))
  args = list(learner = learner, task = task, resampling = resampling, measures = measures, features = features, n.feat.perm = n.feat.perm)
  ret = parallelMap::parallelMap(nullImportanceIteration, i = seq_len(n.target.perm), more.args = args)
  rbindlist(ret, idcol = "n.target.perm")
}
