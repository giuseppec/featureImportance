nullImportanceIteration = function(i, learner, task, resampling, measures, n.feat.perm) {
  d = mlr::getTaskData(task)
  target = mlr::getTaskTargetNames(task)
  # FIXME: maybe do not use unexported changeData here, can be problematic on CRAN
  task.null = mlr:::changeData(task, data = permuteFeature(d, target))
  fi = featureImportance(learner = learner, task.null, resampling = resampling,
    measures = measures, n.feat.perm = n.feat.perm)
  fi$importance
}

nullImportance = function(learner, task, resampling, measures, n.feat.perm = 1, n.target.perm = 1) {
  args = list(learner = learner, task = task, resampling = resampling, measures = measures, n.feat.perm = n.feat.perm)
  ret = parallelMap::parallelMap(nullImportanceIteration, i = seq_len(n.target.perm), more.args = args)
  rbindlist(ret, idcol = "n.target.perm")
}

nullImportanceIterationRF = function(i, learner, task, resampling, measures) {
  d = mlr::getTaskData(task)
  target = mlr::getTaskTargetNames(task)
  # FIXME: maybe do not use unexported changeData here, can be problematic on CRAN
  task.null = mlr:::changeData(task, data = permuteFeature(d, target))
  mod = train(learner, task.null)
  #getFeatureImportance(mod, "permutation", type = 1, scale = FALSE)$res
  as.data.frame(t(importance(mod$learner.model, type = 1, scale = FALSE)))
}

nullImportanceRF = function(learner, task, resampling, measures, n.target.perm = 1) {
  args = list(learner = learner, task = task)
  ret = parallelMap::parallelMap(nullImportanceIterationRF, i = seq_len(n.target.perm), more.args = args)
  rbindlist(ret, idcol = "n.target.perm")
}
