library(OpenML)
library(batchtools)
library(featureImportance)

# create registry
path = "paper/application/application_pfi_bh"
unlink(path, recursive = TRUE)
reg = makeExperimentRegistry(
  file.dir = path,
  packages = c("mlr", "OpenML", "BBmisc", "parallelMap", "featureImportance"),
  source = "paper/application/helper_functions.R",
  seed = 123)
reg$cluster.functions = makeClusterFunctionsSocket(30)

# get data and splits from openml
oml.task = convertOMLTaskToMlr(getOMLTask(167147))
train.ind = oml.task$mlr.rin$train.inds[[1]]

# construct test data
task = oml.task$mlr.task
target = getTaskTargetNames(task)
test = getTaskData(task)[-train.ind, ]

# specify models
lrn = list(
  makeLearner("regr.lm"),
  makeLearner("regr.ksvm"),
  makeLearner("regr.randomForest", importance = TRUE)
)

set.seed(1)
mod = lapply(lrn, function(x) train(x, task, subset = train.ind))
for (m in mod) {
  addProblem(name = getLearnerId(m$learner),
    data = list(mod = m, target = target, test = test), seed = 1)
}

addAlgorithm("pfi", fun = function(job, instance, data, feat) {
  # get Task
  features = feat
  target = data$target
  test = data$test
  mod = data$mod

  # define measures
  measures = list(mse, mae, medae, rmse, medse, pred)
  mid = vcapply(measures, function(x) x$id)

  # measure performance on test data
  unpermuted.perf = featureImportance:::measurePerformance.WrappedModel(mod, data = test,
    target = target, measures = measures, local = TRUE)
  unpermuted.perf$pred = 0

  # create all permutations
  data.perm = cartesian(test, features, target)
  permuted.perf = featureImportance:::measurePerformance.WrappedModel(mod, data = data.perm,
    target = target, measures = measures, predict.fun = predict.fun, local = TRUE)
  permuted.perf$row.id = data.perm$obs.id

  # measure PFI by taking differences
  pfi = lapply(split(permuted.perf, data.perm$replace.id), function(x) {
    imp = featureImportance:::measureFeatureImportance(x, unpermuted.perf)
  })

  # join with feature values
  pfi = lapply(1:length(pfi), function(i)
    cbind(pfi[[i]], feature.value = test[i , features], features = features))
  pfi = rbindlist(pfi, idcol = "replace.id")

  list(
    pfi = cbind(pfi, learner = getLearnerId(mod$learner)),
    unpermuted.perf = unpermuted.perf,
    permuted.perf = permuted.perf
    )
})

# fit model on train data
ades = data.table(
  feat = getTaskFeatureNames(task)
)

addExperiments(algo.designs = list(pfi = ades))
submitJobs(ids = findNotSubmitted(), reg = reg)

# collect results
pfi = rbindlist(reduceResultsList(findDone(), fun = function(x) {
  x$pfi
}))

saveRDS(pfi, file = "paper/application/application_pfi_bh.Rds")
