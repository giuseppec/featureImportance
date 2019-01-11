library(checkpoint)
checkpoint("2018-03-01", project = "ecml-demo/helper")
source("ecml-demo/helper/packages.R")
install()
library(featureImportance)

# create registry
path = "ecml-demo/application_importance_realdata"
unlink(path, recursive = TRUE)
reg = makeExperimentRegistry(
  file.dir = path,
  packages = c("featureImportance"),
  source = paste0("ecml-demo/", c("helper/functions.R", "helper/packages.R")),
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
set.seed(1)
lrn = list(
  makeLearner("regr.ksvm"),
  makeLearner("regr.randomForest", importance = TRUE)
)
mod = lapply(lrn, function(x) train(x, task, subset = train.ind))
for (m in mod) {
  addProblem(name = getLearnerId(m$learner),
    data = list(mod = m, target = target, test = test), seed = 1)
}
saveRDS(mod, file = paste0(path, "_mod.Rds"))

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

  return(imp)
})

# fit model on train data
ades = data.table(
  feat = getTaskFeatureNames(task)
)

addExperiments(algo.designs = list(pfi = ades))

# submit jobs
jobs = getJobTable()
ids = jobs$job.id[jobs$problem == "regr.randomForest"]
submitJobs(ids = ids, reg = reg)

# collect results
pfi = rbindlist(reduceResultsList(findDone()))

saveRDS(pfi, file = paste0(path, ".Rds"))
