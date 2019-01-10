library(OpenML)
library(batchtools)
library(featureImportance)

# create registry
path = "application_importance_realdata"
unlink(path, recursive = TRUE)
reg = makeExperimentRegistry(
  file.dir = path,
  packages = c("mlr", "OpenML", "BBmisc", "featureImportance"),
  source = "helper_functions.R",
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
lrn = makeLearner("regr.randomForest", importance = TRUE)

set.seed(1)
mod = train(lrn, task, subset = train.ind)
addProblem(name = getLearnerId(m$learner),
  data = list(mod = mod, target = target, test = test), seed = 1)

addAlgorithm("pfi", fun = function(job, instance, data, feat) {
  # get Task
  features = feat
  target = data$target
  test = data$test
  mod = data$mod

  # define measures
  measures = mse

  # compute local feature importance by replacing feature values for all test data points
  pfi = featureImportance(mod, data = test, features = list(features),
    target = target, measures = measures, local = TRUE, replace.ids = 1:nrow(test))
  imp = pfi$importance
  #na.ind = imp$replace.id == imp$row.id
  #imp[na.ind, measures$id] = NA

  list(pfi = cbind(imp, learner = getLearnerId(mod$learner)))
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

# subset results
pfi$feature.value = as.numeric(as.character(pfi$feature.value))

saveRDS(pfi, file = paste0(path, ".Rds"))
