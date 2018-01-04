library(devtools)
library(BBmisc)
library(mlr)
load_all("pkg")

set.seed(1)
learner = makeLearner("classif.ranger", predict.type = "prob")
learner = setHyperPars(learner, importance = "permutation", num.trees = 20)
task = convertMLBenchObjToTask("mlbench.xor", n = 1000, d = 2)

# add correlated feature
d = getTaskData(task)
d$x.3 = d$x.1 + rnorm(getTaskSize(task), sd = 0.25)

# add n additional noisy features
n = 2
dim = ncol(d) - 1
X.noisy = setColNames(replicate(n = 2, runif(getTaskSize(task))), paste0("x.", dim + 1:n))
d = cbind(d, X.noisy)
task = makeClassifTask(data = d, target = "classes")

# visualize
plotLearnerPrediction(learner, task)

# fit model and get importance
mod = train(learner, task)
data = getTaskData(task)
target = getTaskTargetNames(task)
getFeatureImportance(mod)

measures = list(acc)
mid = vcapply(measures, function(x) x$id)
features = getTaskFeatureNames(task)
resampling = makeResampleInstance(makeResampleDesc("CV", iter = 3), task)

res = resample(learner, task, resampling, measures, models = TRUE)

# importance
imp = performanceDrop(res, data = d, features = features, measures = measures,
  n.feat.perm = 10, local = FALSE)
f.imp = imp[, lapply(.SD, mean), .SDcols = mid, by = "features"]
f.imp

shapley = shapleyImportance(object = res, data, target = "classes", features = features,
  measures = list(acc, mmce, auc), m = 100, n.feat.perm = 10)
shapley
