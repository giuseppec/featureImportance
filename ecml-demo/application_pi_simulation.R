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
path = "application_pi_simulation"
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
sig = diag(2)
n = 10000
generateY = function(X) {
  eps = rnorm(nrow(X), sd = 0.5)
  form = ~ V1 + V2 + ifelse(V3 == 0, I(10*V1), I(10*V2)) - 1
  mat = model.matrix(form, data = X)
  rowSums(mat) + eps
}
X = as.data.frame(mvrnorm(n, mu = rep(0, ncol(sig)), Sigma = sig))
X$V3 = rbinom(n, size = 1, prob = 0.5)
X$y = generateY(X)
task = makeRegrTask(data = X, target = "y")

# create learners
lrn = makeLearner("regr.randomForest", ntree = 100, importance = TRUE)
measures = list(mse, mae, pred)

# add problems
mod = train(lrn, task)
prob.pars = list(mod = mod, sigma = sig, n = 100, generateY = generateY, measures = measures)
addProblem(name = getLearnerId(lrn), data = prob.pars, seed = 1)

# add algorithms
addAlgorithm("pfi", fun = function(job, instance, data) {
  # get static stuff
  mod = data$mod
  feat = mod$features
  target = getTaskDesc(mod)$target
  generateY = data$generateY
  measures = data$measures
  mid = vcapply(measures, function(x) x$id)
  # create test with repl seed
  set.seed(job$repl)
  test = as.data.frame(mvrnorm(data$n, mu = rep(0, ncol(data$sig)), Sigma = data$sig))
  test$V3 = rbinom(data$n, size = 1, prob = 0.5)
  test$y = generateY(test)

  pfi = lapply(feat, function(features) {
    # measure performance on test data
    unpermuted.perf = featureImportance:::measurePerformance.WrappedModel(mod,
      data = test, target = target, measures = measures, local = TRUE)
    unpermuted.perf$pred = 0

    # create all permutations
    data.perm = cartesian(test, features, target)
    permuted.perf = featureImportance:::measurePerformance.WrappedModel(mod,
      data = data.perm, target = target, measures = measures,
      predict.fun = predict.fun, local = TRUE)
    permuted.perf$row.id = data.perm$obs.id

    # measure PFI by taking differences
    pfi = lapply(split(permuted.perf, data.perm$replace.id), function(x) {
      imp = featureImportance:::measureFeatureImportance(x, unpermuted.perf)
    })

    # join with feature values
    pfi = lapply(1:length(pfi), function(i)
      cbind(pfi[[i]], feature.value = test[i , features], features = features))
    pfi = rbindlist(pfi, idcol = "replace.id")

    return(pfi)
  })
  pfi = setNames(pfi, feat)
  pfi = rbindlist(pfi)
  pfi[is.na(pfi)] = 0

  list(res = pfi, data = test)
})

addExperiments(repls = 100)
submitJobs(ids = findNotSubmitted(), reg = reg)

# get results
res = reduceResultsList(findDone(), fun = function(x, job) {
  x$res
})

saveRDS(res, file = paste0(path, ".Rds"))

for (i in 1:length(res)) {
  res[[i]] = res[[i]][, mae := NULL]
  res[[i]] = res[[i]][, pred := NULL]
}

saveRDS(res, file = paste0(path, ".Rds"))
