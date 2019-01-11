library(checkpoint)
checkpoint("2018-06-01", project = "ecml-demo/helper", forceProject = TRUE)
source("ecml-demo/helper/packages.R")
install()
library(featureImportance)

# create batchtools registry
path = "ecml-demo/application_pi_simulation"
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

# add problems
mod = train(lrn, task)
prob.pars = list(mod = mod, sigma = sig, n = 100, generateY = generateY, measures = mlr::mse)
addProblem(name = getLearnerId(lrn), data = prob.pars, seed = 1)

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
  test = as.data.frame(mvrnorm(data$n, mu = rep(0, ncol(data$sig)), Sigma = data$sig))
  test$V3 = rbinom(data$n, size = 1, prob = 0.5)
  test$y = generateY(test)

  pfi = lapply(feat, function(features) {
    imp = featureImportance(mod, data = test, features = list(features),
      target = target, measures = measures, local = TRUE, replace.ids = 1:nrow(test))
    return(imp$importance)
  })
  pfi = setNames(pfi, feat)
  pfi = rbindlist(pfi)

  list(res = pfi, data = test)
})

addExperiments(repls = 100)
submitJobs(ids = findNotSubmitted(), reg = reg)

# get results
res = reduceResultsList(findDone(), fun = function(x, job) {
  x$res
})

saveRDS(mod, file = paste0(path, "_mod.Rds"))
saveRDS(res, file = paste0(path, ".Rds"))
