library(checkpoint)
checkpoint("2018-03-01", project = "ecml-demo/helper", forceProject = TRUE)
source("ecml-demo/helper/packages.R")
install()
library(featureImportance)

# create batchtools registry
path = "ecml-demo/application_shapley_simulation"
unlink(path, recursive = TRUE)
reg = makeExperimentRegistry(
  file.dir = path,
  packages = c("featureImportance"),
  source = paste0("ecml-demo/", c("helper/functions.R", "helper/packages.R")),
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

  pfi = pfi(mod, data = X, target, measures, features = feat)

  pfi.diff = pfi$pfi.diff$importance[, lapply(.SD, mean), by = "features"]
  pfi.ratio = pfi$pfi.ratio$importance[, lapply(.SD, mean), by = "features"]

  pfi.diff = cbind(pfi.diff[, .(feature = features, mse = mse)], method = "pfi.diff")
  pfi.ratio = cbind(pfi.ratio[, .(feature = features, mse = mse)], method = "pfi.ratio")

  res = rbind(pfi.diff, pfi.ratio)
  list(res = res, pfi = pfi, data = X)
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

  shapley = shapleyImportance(mod, data = X, value.function = vGE,
    target = target, measures = measures, features = feat)
  res = cbind(shapley$shapley.value, method = "shapley")

  list(res = res, shapley = shapley, data = X)
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
  res = ge(mod, data = X, target, measures, feat)
  res = cbind(rbindlist(res, idcol = "method"), feature = names(res))

  list(res = res, data = X)
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
res.cleanup = lapply(1:length(res), function(i) {
  cbind(setcolorder(res[[i]], cols), learner = learner[i], repl = repl[i])
})
res.cleanup = rbindlist(res.cleanup)
saveRDS(res.cleanup, file = paste0(path, ".Rds"))
