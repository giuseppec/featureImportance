library(devtools)
library(BBmisc)
library(mlr)
load_all("pkg")

set.seed(1)
learner = makeLearner("classif.randomForest", predict.type = "prob")
learner = setHyperPars(learner, importance = TRUE, ntree = 20)
task = convertMLBenchObjToTask("mlbench.xor", n = 1000, d = 2)

# add correlated feature
d = getTaskData(task)
d$x.3 = d$x.1 + rnorm(getTaskSize(task), sd = 0.25)

# add n additional noisy features
n = 2
dim = ncol(d) - 1
X.noisy = setColNames(replicate(n = 2, runif(getTaskSize(task))), paste0("x.", dim + 1:n))
d = cbind(d, X.noisy)
task = makeClassifTask(data = d, target = "classes") # pid.task

resampling = makeResampleInstance(makeResampleDesc("CV", iter = 3, stratify = TRUE), task)

library(parallelMap)
parallelStartSocket(30)
#parallelExport("permuteFeature", "featureImportance", "performanceDrop")
parallelLibrary("checkmate", "BBmisc", "mlr", "featureImportance")
feat.names = getTaskFeatureNames(task)
feat.imp = featureImportance(learner, task, resampling, measures, n.feat.perm = 1000)
imp = feat.imp$importance[, lapply(.SD, mean), .SDcols = "acc", by = c("features")]
imp$lo = feat.imp$importance[, lapply(.SD, quantile, c(0.05)), .SDcols = "acc", by = c("features")]$acc
imp$up = feat.imp$importance[, lapply(.SD, quantile, c(0.95)), .SDcols = "acc", by = c("features")]$acc
imp

null = nullImportance(learner, task, resampling, measures, n.feat.perm = 10, n.target.perm = 100)
null2 = null[, lapply(.SD, mean), .SDcols = "acc", by = c("features", "n.feat.perm", "n.target.perm")]
#null3 = null[, lapply(.SD, mean), .SDcols = "acc", by = c("features", "n.target.perm")]

null4 = nullImportance(learner, task, resampling, measures, n.feat.perm = 1, n.target.perm = 1000)
null5 = null4[, lapply(.SD, mean), .SDcols = "acc", by = c("features", "n.feat.perm", "n.target.perm")]
parallelStop()

pvalue = function(null, imp, measure.id = "acc") {
  null = as.data.frame(null)
  imp = as.data.frame(imp)
  sp = split(null[, measure.id], null$features)

  ret = setNames(lapply(imp$features, function(x) {
    true = imp[imp$feature == x, measure.id]
    n = sp[[x]]

    list(
      p.value = mean(n > true),
      null.dist = n,
      true = true
    )
  }), imp$features)
  return(ret)
}

p = pvalue(null5, imp)
sort(vnapply(p, function(x) x$p.value))

i = 3
plot(density(p[[i]]$null.dist), main = names(p)[i])
abline(v = p[[i]]$true, col = "red")



library(vita)
mod = train(learner, task)
d = getTaskData(task)
target = getTaskTargetNames(task)
imp = getFeatureImportance(mod, "permutation")
imp

pimp = vita:::PIMP.default(d[, !colnames(d) %in% target], d[, target], mod$learner.model, S = 1000)
sort(pimp$VarImp[, "VarImp"])
test = vita:::PimpTest.default(pimp)
test = cbind(test$VarImp, test$pvalue)
test[order(test[, "p-value"], decreasing = TRUE),]


res = resample(learner, task, resampling, measures, models = TRUE)

imp = performanceDrop(object = res, data = d, features = features, measures = measures)
imp[, lapply(.SD, mean), .SDcols = "acc", by = c("features")]

#parallelStartSocket(30)
shapley = shapleyImportance(object = res, data = d, target = target, features = features,
  measures = list(acc, mmce, auc), m = 300, n.feat.perm = 1)
#parallelStop()
shapley
val = sortByCol(shapley$shapley.value, "features")
ser = sortByCol(shapley$shapley.uncertainty, "features")
sort(setNames(val$acc/ser$acc, val$features))
val$lo = val$acc - 2*ser$acc
val$up = val$acc + 2*ser$acc
val
