library(devtools)
library(BBmisc)
library(checkmate)
library(data.table)
library(parallelMap)
library(ggplot2)
library(plotly)
#install("pkg")
library(featureImportance)
set.seed(1)
#learner = makeLearner("classif.ranger", num.trees = 100, predict.type = "prob")
#learner = setHyperPars(learner, importance = "permutation", scale.permutation.importance = TRUE)
#learner = makeLearner("classif.randomForest", ntree = 100, predict.type = "prob", importance = TRUE)
learner = makeLearner("classif.rpart", predict.type = "prob")
task = pid.task
measures = acc
weights = NULL
features = getTaskFeatureNames(task)
resampling = makeResampleInstance(makeResampleDesc("CV", iter = 3), task)

mod = train(learner, task)
d = getTaskData(task)
target = getTaskTargetNames(task)
imp = getFeatureImportance(mod, "permutation")

#res = mlr::resample(learner, task, resampling, measures, weights, models = TRUE, keep.pred = FALSE)

# our CV permutation importance
library(parallelMap)
parallelStartSocket(30)
#parallelExport("permuteFeature", "featureImportance", "performanceDrop")
parallelLibrary("checkmate", "BBmisc", "mlr", "featureImportance")
feat.names = getTaskFeatureNames(task)
feat.imp = featureImportance(learner, task, resampling, measures, n.feat.perm = 10)

null.x = nullImportance(learner, task, resampling, measures, n.feat.perm = 100, n.target.perm = 1)
null.x = aggregate(acc ~ features + n.target.perm + n.feat.perm, data = null.x, mean)

null.y = nullImportance(learner, task, resampling, measures, n.feat.perm = 1, n.target.perm = 100)
null.y = aggregate(acc ~ features + n.target.perm + n.feat.perm, data = null.y, mean)

null.xy = nullImportance(learner, task, resampling, measures, n.feat.perm = 100, n.target.perm = 100)
null.xy = aggregate(acc ~ features + n.target.perm + n.feat.perm, data = null.xy, mean)
parallelStop()


# aggregate all feat perm and compare with case when x is only permuted once
null.xy2 = aggregate(acc ~ features + n.target.perm, data = null.xy, mean)
nullImp = rbind(
  cbind(null.xy, method = "xy perm"),
  cbind(null.x, method = "x perm"),
  cbind(null.y, method = "y perm")
)

p = ggplot(data = nullImp, aes(x = acc, fill = method)) +
  geom_density() + facet_wrap(~ features, scales = "free")
ggplotly(p)


parallelStartSocket(30)
#parallelExport("permuteFeature", "featureImportance", "performanceDrop")
parallelLibrary("checkmate", "BBmisc", "mlr", "featureImportance")
feat.names = getTaskFeatureNames(task)

null.yperm = nullImportance(learner, task, resampling, measures, n.feat.perm = 1, n.target.perm = 100)
null.yperm = aggregate(acc ~ features + n.target.perm + n.feat.perm, data = null.yperm, mean)

null.xyperm = nullImportance(learner, task, resampling, measures, n.feat.perm = 10, n.target.perm = 50)
null.xyperm = aggregate(acc ~ features + n.target.perm + n.feat.perm, data = null.xyperm, mean)
parallelStop()


null.xyperm2 = aggregate(acc ~ features + n.target.perm, data = null.xyperm, mean)
nullImp = rbind(
  cbind(null.xyperm2, n.feat.perm = 1, method = "xy perm"),
  cbind(null.yperm, method = "y perm")
)

p = ggplot(data = nullImp, aes(x = acc, fill = method)) +
  geom_density() + facet_wrap(~ features, scales = "free")
ggplotly(p)

# # aggreagte all
# null.xy3 = aggregate(acc ~ features, data = null.xy, mean)
# p = ggplot(data = null.x, aes(x = acc, fill = features)) + geom_density(alpha = 0.25)
# p = p + geom_density(data = null.xy3, alpha = 0.75) + facet_wrap(~ features, scales = "free")
# p
#
# p = ggplot(data = null.xy2, aes(x = acc, fill = features)) +
#   geom_density(data = null.xy2, alpha = 0.5)
# p

imp = aggregate(acc ~ features, data = feat.imp$importance, FUN = mean)

pvalue = function(null, imp) {
  sp = split(null$acc, null$features)

  sort(vnapply(imp$features, function(x) {
    true = imp$acc[imp$feature == x]
    #n = unlist(null2)
    n = sp[[x]]

    plot(density(n), main = x)
    abline(v = true, col = "red")
    mean(n > true)
  }))
}

pvalue(null.y, imp)
pvalue(null.xy2, imp)
pvalue(null.x, imp)



imp.dat = feat.imp$importance
ggplot(data = imp.dat, aes(x = features, y = acc)) +
  geom_violin() +
  geom_boxplot(aes(fill = factor(cv.iter))) +
  facet_wrap(~ features, scales = "free")

imp.dat2 = as.data.table(aggregate(acc ~ features + n.feat.perm, data = imp.dat, FUN = mean))
ggplot(data = imp.dat2, aes(x = features, y = acc)) +
  geom_violin() +
  geom_boxplot()

imp.dat3 = as.data.table(aggregate(acc ~ features + cv.iter, data = imp.dat, FUN = mean))
ggplot(data = imp.dat3, aes(x = features, y = acc)) +
  geom_violin() +
  geom_boxplot()

# feat.imp = unlist(feat.imp, recursive = FALSE)
# feat.imp.raw = rbindlist(feat.imp)
#
# feat.imp.repeated = lapply(feat.imp, colMeans)
# feat.imp.repeated = setNames(transpose(feat.imp.repeated), feat.names)
#
# #par(mfrow = c(2, 4), mar = c(3,3,3,1))
# #lapply(feat.imp.repeated, function(x) plot(density(x)))
#
# feat.imp.mean = vnapply(feat.imp.repeated, mean)#/vnapply(feat.imp.repeated, sd)
# #lattice::dotplot(sort(feat.imp.mean))
#
# feat.imp.repeated.data = melt(cbind(as.data.frame(feat.imp.repeated), iter = row.names(as.data.frame(feat.imp.repeated))))
# feat.imp.repeated.data = cbind(feat.imp.repeated.data[,-1], method = paste0("perm"))

bla = aggregate(acc ~ features, imp.dat, mean)
bla$acc = bla$acc/sum(bla$acc)
feat.dat = rbind(
  data.frame(acc = unlist(imp$res/sum(imp$res)), feature = colnames(imp$res), method = "rpart"),
  cbind(bla, method = "perm")
  #feat.imp.repeated.data[,c("value", "variable", "method")]
  #data.frame(value = feat.imp.mean, variable = names(feat.imp.mean), method = "permutation")
)

# aggregate(auc ~ feature, aggregate(auc ~ feature + n.feat.perm, feat.imp, mean), function(x) mean(x)/sd(x))

p = ggplot(data = feat.dat, aes(x = acc, y = reorder(feature, acc))) +
  geom_point(aes(col = method, alpha = 0.5)) +
  theme_minimal()
ggplotly(p)



parallelStartSocket(30)
parallelExport("permuteFeature", "featureImportance", "performanceDrop")
parallelLibrary("checkmate", "BBmisc", "mlr")
null = nullImportance(learner, task, resampling, measures, n.feat.perm = 10, n.target.perm = 90)
parallelStop()



null2 = rbindlist(null, idcol = "n.target.perm")
null2 = aggregate(acc ~ feature + n.target.perm, data = null2, mean)
#null2$n.feat.perm = NULL
null2 = split(null2$acc, null2$feature)

feat.imp2 = aggregate(acc ~ feature, data = feat.imp, mean)

par(mfrow = c(2, 4), mar = c(3,3,3,1))
sort(vnapply(names(null2), function(x) {
  true = feat.imp2$acc[feat.imp2$feature == x]
  n = unlist(null2)
  #n = null2[[x]]

  plot(density(n), main = x)
  abline(v = true, col = "red")
  mean(n > true)
}))




################

PIMP.default = function(X, y, rForest, S = 100, parallel = FALSE, ncores = 0,
  seed = 123, ...)
{
  if (!inherits(rForest, "randomForest"))
    stop("rForest is not of class randomForest")
  mtry = rForest$mtry
  ntree = rForest$ntree
  n = nrow(X)
  p = ncol(X)
  if (Sys.info()[["sysname"]] == "Windows" & parallel) {
    cat("\n The parallelized version of the PIMP-algorithm are not available on Windows !! \n")
    parallel = FALSE
  }
  if (parallel) {
    d_ncores = parallel::detectCores()
    if (ncores > d_ncores)
      stop("ncores: The number of cores is too large")
    if (ncores == 0) {
      ncores = max(c(1, floor(d_ncores/2)))
    }
    if ("L'Ecuyer-CMRG" != RNGkind()[1]) {
      cat("\n The random number generator was set to L'Ecuyer-CMRG !! \n")
      RNGkind("L'Ecuyer-CMRG")
    }
  }
  else {
    ncores = 1
  }
  set.seed(seed)
  classRF = is.factor(y)
  if (classRF) {
    if (rForest$type == "regression")
      stop("rForest$type = regression !! y a factor ")
    y.s = replicate(S, sample(as.integer(y)))
    i = 1:S
    varip = parallel::mclapply(i, function(i) {
      randomForest::randomForest(X, as.factor(y.s[, i]),
        mtry = mtry, importance = TRUE, ntree = ntree,
        ...)[[9]][, 3]
    }, mc.cores = ncores)
    varip = simplify2array(varip)
  }
  else {
    if (rForest$type == "classification")
      stop("rForest$type = classification !! y not a factor ")
    y.s = replicate(S, sample(y))
    i = 1:S
    varip = parallel::mclapply(i, function(i) {
      randomForest::randomForest(X, y.s[, i], mtry = mtry,
        importance = TRUE, ntree = ntree, ...)[[7]][, 3]
    }, mc.cores = ncores)
    varip = simplify2array(varip)
  }
  dimNames = dimnames(rForest$importance)[[1]]
  out = list(VarImp = matrix(rForest$importance[, 3], ncol = 1,
    dimnames = list(dimNames, "VarImp")), PerVarImp = varip,
    type = if (classRF) {
      "classification"
    } else {
      "regression"
    }, call = match.call())
  class(out) = "PIMP"
  return(out)
}

library(vita)
learner = makeLearner("classif.randomForest", predict.type = "prob", ntree = 25)
task = pid.task
measures = acc
weights = NULL
features = getTaskFeatureNames(task)
resampling = makeResampleInstance(makeResampleDesc("CV", iter = 3), task)

mod = train(learner, task)
d = getTaskData(task)
target = getTaskTargetNames(task)
imp = getFeatureImportance(mod, "permutation")

pimp = vita:::PIMP.default(d[, !colnames(d) %in% target], d[, target], mod$learner.model, S = 90)
sort(pimp$VarImp[, "VarImp"])
pimp = vita:::PimpTest.default(pimp)
pimp = cbind(pimp$VarImp, pimp$pvalue)
pimp[order(pimp[, "p-value"], decreasing = TRUE),]

library(Boruta)
b = Boruta(diabetes ~ ., data = d)


feat.names = getTaskFeatureNames(task)
feat.imp = featureImportance(learner, task, resampling, measures, n.feat.perm = 10)
null.y = nullImportance(learner, task, resampling, measures, n.feat.perm = 10, n.target.perm = 50)





library(mlbench)
data("Ozone")
str(Ozone)
Ozone = na.omit(Ozone)
ozone.task = makeRegrTask(data = Ozone, target = "V4")
b = Boruta(V4 ~ ., data = Ozone)
b

learner = makeLearner("regr.ranger", num.trees = 100)
learner = setHyperPars(learner, importance = "permutation", scale.permutation.importance = TRUE)
measures = mse
resampling = makeResampleInstance(makeResampleDesc("CV"), ozone.task)
feat.imp = featureImportance(learner, ozone.task, resampling, measures, n.feat.perm = 10)

parallelStartSocket(30)
parallelExport("permuteFeature", "featureImportance", "performanceDrop")
parallelLibrary("checkmate", "BBmisc", "mlr")
null = nullImportance(learner, ozone.task, resampling, measures, n.feat.perm = 10, n.target.perm = 90)
parallelStop()

null2 = rbindlist(null, idcol = "n.target.perm")
null2 = aggregate(mse ~ feature + n.target.perm, data = null2, mean)
null2 = split(null2$mse, null2$feature)
#null2$n.feat.perm = NULL

feat.imp2 = aggregate(mse ~ feature, data = feat.imp, mean)

par(mfrow = c(2, 4), mar = c(3,3,3,1))
sort(vnapply(names(null2), function(x) {
  true = feat.imp2$mse[feat.imp2$feature == x]
  #n = unlist(null2)
  n = null2[[x]]

  plot(density(n), main = x)
  abline(v = true, col = "red")
  mean(n > true)
}))

bla = aggregate(mse ~ feature, feat.imp, mean)
bla[order(bla$mse, decreasing = TRUE),]
