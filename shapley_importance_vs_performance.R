library(devtools)
library(BBmisc)
library(mlr)
library(data.table)
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

mod = train(learner, task)
data = getTaskData(task)
features = getTaskFeatureNames(task)
target = getTaskTargetNames(task)

# calculateValueFunction = function(features){
#   val = list(c("1", "3"), c("2", "3"), c("1", "2", "3"))
#   if (any(vlapply(val, function(x) all(x %in% features))))
#     return(1) else
#       return(0)
# }

perm = generatePermutations(features)
mc = generateMarginalContribution("x.3", perm)
as.data.table(mc)



mc = c(
  generateMarginalContribution("x.1", perm),
  generateMarginalContribution("x.2", perm),
  generateMarginalContribution("x.3", perm),
  generateMarginalContribution("x.4", perm))
values = unique(unname(unlist(mc, recursive = FALSE)))

value.function = lapply(values, function(f) {
  calculateValueFunction(object = mod, data = data, measures = measures,
    n.feat.perm = 5, local = FALSE, features = f)
})
vf = rbindlist(value.function)
vf$features = values

value.function2 = lapply(values, function(f) {
  calculateValueFunction2(object = mod, data = data, measures = measures,
    n.feat.perm = 5, local = FALSE, features = f)
})
vf2 = rbindlist(value.function2)
vf2$features = values

mc = generateMarginalContribution("x.4", perm)
val = getMarginalContributionValues(mc, vf)
val2 = getMarginalContributionValues(mc, vf2)

getShapleyImportance(val, measures = measures)
getShapleyImportance(val2, measures = measures)

#vnapply(mc, function(x) calculateValueFunction(x$with.f) - calculateValueFunction(x$without.f))
#mean(vnapply(mc, function(x) calculateValueFunction(x$with.f) - calculateValueFunction(x$without.f)))

