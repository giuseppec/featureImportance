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

### Plot results
res.cleanup = readRDS("application_shapley_simulation.Rds")
res.cleanup = subset(res.cleanup, method %in% c("pfi.diff", "pfi.ratio", "shapley"))
# compute percentages for 3 features
res.cleanup[, ratio := mse/mse[feature == "V3"], by = c("method", "learner", "repl")]
res.cleanup = subset(res.cleanup, feature != "V3")

lab.lrn = function(lrn) gsub("regr.", "", lrn)
pp = ggplot(data = res.cleanup, aes(x = feature, y = ratio))
pp = pp + geom_boxplot(aes(fill = method), lwd = 0.25, outlier.size = 0.75)
pp = pp + facet_grid(. ~ learner, scales = "free", labeller = labeller(learner = lab.lrn))
pp = pp +
  scale_fill_discrete(name = "method",  labels = c("PFI (Diff.)", "PFI (Ratio)", "SFIMP"))  +
  scale_fill_grey(start = 0.4, end = .95) +
  theme_minimal() +
  theme(legend.text = element_text(size = rel(0.6), angle = 0),
    axis.text.y = element_text(angle = 0))
pp







### Plot Example
res.cleanup = readRDS("application_shapley_simulation.Rds")
# specify colour and legend text
feat.order = c("V3", "V2", "V1", "geP")
col = c(gray.colors(3, start = 0.5, end = .9), hcl(h = 195, l = 65, c = 100))
col = setNames(col, feat.order)
legend.lab = c(
  "V1" = bquote(phi[1]),
  "V2" = bquote(phi[2]),
  "V3" = bquote(phi[3]),
  "geP" = bquote(widehat(GE)[P])
)
# select one single run of the simulation with 500 repetitions to plot the example
res.ex = subset(res.cleanup, repl == 2 & method %in% c("shapley", "geP"), select = c("learner", "feature", "mse", "method"))
# change sign
res.ex[, mse := ifelse(method != "geP", -mse, mse)]
# reorder features for plotting
res.ex$feature = factor(res.ex$feature, levels = feat.order)
# shorten string for learner
res.ex$learner = factor(gsub("regr.", "", res.ex$learner))
# add column containing proportion of explained importance
res.ex[, perc := ifelse(feature == "geP", NA, mse[feature != "geP"]/sum(mse[feature != "geP"])*100), by = c("learner")]
# add column containing drop in MSE + proportion of explained importance
res.ex[, label := ifelse(feature == "geP", round(mse, 2), paste0(round(mse, 2), " (", round(perc, 0), "%)"))]

plot.ex = ggplot(res.ex, aes(x = learner, y = mse, fill = feature))
plot.ex = plot.ex + geom_bar(stat = "identity", colour = "white", pos = "stack") + coord_flip()
plot.ex = plot.ex + geom_text(aes(label = label), position = position_stack(vjust = 0.5), size = 2.5)
plot.ex = plot.ex +
  ylab("performance (MSE)") +
  xlab("") +
  ggtitle("(a) Comparing the model performance and SFIMP values across different models") +
  scale_fill_manual(values = col, name = " performance \n explained by", labels = legend.lab) +
  theme(legend.key.size = unit(0.75, "line"), title = element_text(size = 8), axis.title = element_text(size = 8),
    axis.text.y = element_text(size = 7))

plot.ex

