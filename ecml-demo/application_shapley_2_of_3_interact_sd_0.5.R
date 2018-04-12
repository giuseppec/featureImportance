library(mlr)
library(mmpf)
library(BBmisc)
library(data.table)
library(devtools)
library(ggplot2)
library(checkmate)
library(data.table)
library(stringi)
library(MASS)
library(featureImportance)
library(batchtools)
#load_all("../featureImportance")

# create registry
id = "application_shapley_2_of_3_interact_sd_0.5"
model.file = paste0("paper/application/", id, "_models.Rds")
path = paste0("paper/application/", id)
unlink(path, recursive = TRUE)
reg = makeExperimentRegistry(
  file.dir = path,
  packages = c("mlr", "BBmisc", "parallelMap", "featureImportance", "MASS", "checkmate", "stringi"),
  source = "paper/application/helper_functions.R",
  seed = 123)
reg$cluster.functions = makeClusterFunctionsSocket(30)

# specify data
set.seed(1)
d = diag(3)
addCorr = function(d, cor = 0) {
  replace(d, d == 0, cor)
}
sig = list(
  cor00 = addCorr(d, 0)#,
  #cor40 = addCorr(d, 0.4),
  #cor50 = addCorr(d, 0.5)
)
n = 10000
generateY = function(X) {
  rowSums(model.matrix(~ . + V1:V2 - 1, data = X)) + rnorm(nrow(X), sd = 0.5)
  #rowSums(X) + rnorm(nrow(X), sd = 0.1)
}
tasks = lapply(sig, function(x) {
  X = as.data.frame(mvrnorm(n, mu = rep(0, ncol(x)), Sigma = x))
  X$y = generateY(X)
  makeRegrTask(data = X, target = "y")
})

# create learners
lrns = list(
  makeLearner("regr.lm"),
  makeLearner("regr.rsm", modelfun = "TWI"),
  makeLearner("regr.ksvm"),
  makeLearner("regr.randomForest", ntree = 100, importance = TRUE)
)

# make/load models
mod = vector("list", length(lrns)*length(tasks))

# add problems
ind = 1
pid = character(length(lrns)*length(tasks))
for (lrn in lrns) {
  for (task.name in names(tasks)) {
    # create model if not already stored
    mod[[ind]] = train(lrn, tasks[[task.name]])
    addProblem(name =  paste0(getLearnerId(lrn), task.name), data = list(
      mod = mod[[ind]],
      sigma = sig[[task.name]],
      n = 100,
      generateY = generateY,
      measures = list(mse, mae, medae, rmse, medse)
    ), seed = ind)
    # increment counter
    ind = ind + 1
  }
}
# save models
saveRDS(mod, file = model.file)

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

#addExperiments(repls = 100)
addExperiments(repls = 100)
submitJobs(ids = findNotSubmitted(), reg = reg)

# get results
res = reduceResultsList(findDone(), fun = function(x, job) {
  x$res
})
job = getJobTable()
repl = job$repl
cor = as.numeric(gsub(".*cor", "", job$problem))/100
learner = gsub("cor.*", "", job$problem)
measures = list(mse, mae, medae, rmse, medse)
mid = vcapply(measures, function(x) x$id)
cols = c("method", "feature", mid)
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
  cbind(res.cleanup[[i]], learner = learner[i], cor = cor[i], repl = repl[i])
  })
res.cleanup = rbindlist(res.cleanup)
saveRDS(res.cleanup, file = paste0("paper/application/", id, ".Rds"))



### Plot results
#res.cleanup = readRDS("paper/application/application_shapley_2_of_3_interact.Rds")
(res.perf = subset(res.cleanup, method %in% c("ge0", "geP")))
res.cleanup = subset(res.cleanup, method %in% c("pfi.diff", "pfi.ratio", "shapley"))

# compute percentages for 3 features
res.cleanup[, perc := mse/mse[feature == "V3"], #100*mse/sum(mse),
  by = c("method", "learner", "repl", "cor")] #perc := .SD/sum(.SD), .SDcols = c("mse"),
# res.cleanup[, perc.mae := ifelse(feature == "V3", abs(perc - 25), abs(perc - 37.5)) ]

lab.cor = function(cor) paste("Cor.", cor)
lab.lrn = function(lrn) gsub("regr.", "", lrn)
pp = ggplot(data = res.cleanup, aes(x = feature, y = perc))
pp = pp + geom_boxplot(aes(fill = method), lwd = 0.25, outlier.size = 0.75)
pp = pp + facet_grid(cor ~ learner, scales = "free",
  labeller = labeller(cor = lab.cor, learner = lab.lrn))
# = pp + geom_hline(yintercept = c(100/3)) #+ geom_hline(yintercept = c(25))
pp = pp +
  scale_fill_discrete(name = "method",  labels = c("PFI (Diff.)", "PFI (Ratio)", "SFIMP"))  +
  scale_fill_grey(start = 0.4, end = .95) +
  theme_minimal() +
  theme(legend.text = element_text(size = rel(0.6), angle = 0),
    axis.text.y = element_text(angle = 0))
pp
