library(devtools)
library(BBmisc)
library(mlr)
load_all("pkg")

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
task = makeClassifTask(data = cbind(d, X.noisy), target = "classes")

#
library(OpenML)
#task = pid.task
#task = convertOMLDataSetToMlr(getOMLDataSet(150))

d = getTaskData(task)
#resampling = makeResampleInstance(makeResampleDesc("CV", iter = 10, stratify = TRUE), task)
resampling = makeResampleInstance(makeResampleDesc("RepCV", fold = 5, rep = 5, stratify = TRUE), task)

library(parallelMap)
parallelStartSocket(30)
#parallelExport("permuteFeature", "featureImportance", "performanceDrop")
parallelLibrary("checkmate", "BBmisc", "mlr", "featureImportance")
feat.names = getTaskFeatureNames(task)
feat.imp = featureImportanceLearner(learner, task, resampling, measures, n.feat.perm = 100)
imp = feat.imp$importance[, lapply(.SD, mean), .SDcols = "acc", by = c("features")]
imp

null = nullImportance(learner, task, resampling, measures, n.feat.perm = 100, n.target.perm = 100)
null2 = null[, lapply(.SD, mean), .SDcols = "acc", by = c("features", "n.feat.perm", "n.target.perm")]
parallelStop()

p = pvalue(null2, imp, measure.id = "acc")
(p = sort(vnapply(p, function(x) x$p.value)))

i = 3
plot(density(p[[i]]$null.dist), main = names(p)[i])
abline(v = p[[i]]$true, col = "red")

# Importance using RF internal's feature importance
mod = train(learner, task)
#imp.intern = getFeatureImportance(mod, "permutation", type = 1)$res
#imp.intern = melt(imp.intern, measure.vars = getTaskFeatureNames(task), variable.name = "features")
imp.intern = setColNames(as.data.frame(importance(getLearnerModel(mod), type = 1, scale = FALSE)), "value")
imp.intern$features = row.names(imp.intern)

null.intern = nullImportanceRF(learner, task, n.target.perm = 100)
null.intern2 = melt(null.intern, measure.vars = getTaskFeatureNames(task), variable.name = "features")
null.intern2

p.intern = pvalue(null.intern2, imp.intern, measure.id = "value")
(p.intern = sort(vnapply(p.intern, function(x) x$p.value)))

# Importance using vita
library(vita)
mod = train(learner, task)
target = getTaskTargetNames(task)
d = getTaskData(task)
features = getTaskFeatureNames(task)

# res = resample(learner, task, resampling, measures, models = TRUE)
# imp.vita = performanceDrop(object = res, data = d, features = features, measures = measures)
# imp.vita = imp.vita[, lapply(.SD, mean), .SDcols = "acc", by = c("features")]
# imp.vita = sortByCol(imp.vita, "features")
# merge(imp.vita, imp.intern)

pimp = vita:::PIMP.default(d[, !colnames(d) %in% target], d[, target], mod$learner.model, S = 100)
sort(pimp$VarImp[, "VarImp"])
imp.vita = vita:::PimpTest.default(pimp)
imp.vita = cbind(imp.vita$VarImp, imp.vita$pvalue)
imp.vita[order(imp.vita[, "p-value"], decreasing = TRUE),]

compare = as.data.frame(imp.vita[order(imp.vita[, "p-value"], decreasing = TRUE),])
compare$imp = p[row.names(compare)]
compare$imp.intern = p.intern[row.names(compare)]
compare

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
