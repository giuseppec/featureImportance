library(mlr)
library(BBmisc)
library(data.table)
library(ggplot2)
library(checkmate)
library(data.table)
library(stringi)
library(MASS)
library(featureImportance)
library(batchtools)

# create batchtools registry
path = "application_shapley_simulation"
unlink(path, recursive = TRUE)
reg = makeExperimentRegistry(
  file.dir = path,
  packages = c("mlr", "BBmisc", "featureImportance", "MASS", "checkmate", "stringi"),
  source = "helper_functions.R",
  seed = 123)
# uncomment this line to run experiments in parallel
reg$cluster.functions = makeClusterFunctionsSocket(30)

# specify data
set.seed(1)
sig = diag(3)
n = 10000
generateY = function(X) {
  rowSums(model.matrix(~ . + V1:V2 - 1, data = X)) + rnorm(nrow(X), sd = 0.5)
}
X = as.data.frame(mvrnorm(n, mu = rep(0, ncol(sig)), Sigma = sig))
X$y = generateY(X)
task = makeRegrTask(data = X, target = "y")

# create learners
lrns = list(
  makeLearner("regr.lm"),
  makeLearner("regr.rsm", modelfun = "TWI"),
  makeLearner("regr.ksvm"),
  makeLearner("regr.randomForest", ntree = 100, importance = TRUE)
)

# add problems
for (i in seq_along(lrns)) {
  lrn = lrns[[i]]
  mod = train(lrn, task)
  prob.pars = list(mod = mod, sigma = sig, n = 100, generateY = generateY, measures = mlr::mse)
  addProblem(name = getLearnerId(lrn), data = prob.pars, seed = i)
}

# add algorithms
addAlgorithm("pfi", fun = function(job, instance, data) {
  # get static stuff
  mod = data$mod
  feat = mod$features
  target = getTaskDesc(mod)$target
  generateY = data$generateY
  measures = data$measures
  # create test with repl seed
  set.seed(job$repl)
  X = as.data.frame(mvrnorm(data$n, mu = rep(0, ncol(data$sig)), Sigma = data$sig))
  X$y = generateY(X)

  list(res = pfi(mod, data = X, target, measures, features = feat), data = X)
})

addAlgorithm("shapley", fun = function(job, instance, data) {
  # get static stuff
  mod = data$mod
  feat = mod$features
  target = getTaskDesc(mod)$target
  generateY = data$generateY
  measures = data$measures
  # create test with repl seed
  set.seed(job$repl)
  X = as.data.frame(mvrnorm(data$n, mu = rep(0, ncol(data$sig)), Sigma = data$sig))
  X$y = generateY(X)

  list(res = shapleyImportance(mod, data = X, value.function = vGE,
    target = target, measures = measures, features = feat), data = X)
})

addAlgorithm("ge", fun = function(job, instance, data) {
  # get static stuff
  mod = data$mod
  feat = mod$features
  target = getTaskDesc(mod)$target
  generateY = data$generateY
  measures = data$measures
  # create test with repl seed
  set.seed(job$repl)
  X = as.data.frame(mvrnorm(data$n, mu = rep(0, ncol(data$sig)), Sigma = data$sig))
  X$y = generateY(X)

  list(res = ge(mod, data = X, target, measures, feat), data = X)
})

addExperiments(repls = 500)
submitJobs(ids = findNotSubmitted(), reg = reg)

# get results
res = reduceResultsList(findDone(), fun = function(x, job) {
  x$res
})

job = getJobTable()
repl = job$repl
learner = job$problem
cols = c("method", "feature", "mse")
res.cleanup = lapply(res, function(x) {
  if ("V1" %in% names(x))
    res = rbindlist(lapply(x, rbindlist, idcol = "method"), idcol = "feature") else
      if (inherits(x, "ShapleyImportance"))
        res = cbind(x$shapley.value, method = "shapley") else {
          res = cbind(rbindlist(x, idcol = "method"), feature = names(x))
        }
  setcolorder(res, cols)
})
res.cleanup = lapply(1:length(res.cleanup), function(i) {
  cbind(res.cleanup[[i]], learner = learner[i], repl = repl[i])
  })
res.cleanup = rbindlist(res.cleanup)
saveRDS(res.cleanup, file = paste0(path, ".Rds"))
