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
measures = list(acc, mmce)
mid = BBmisc::vcapply(measures, function(x) x$id)

# calculateValueFunction = function(features){
#   val = list(c("1", "3"), c("2", "3"), c("1", "2", "3"))
#   if (any(vlapply(val, function(x) all(x %in% features))))
#     return(1) else
#       return(0)
# }

# generate all permutations
perm = generatePermutations(features)

# generate all marginal contribution sets for each feature
mc = lapply(features, function(x) generateMarginalContribution(x, perm))
mc = unlist(mc, recursive = FALSE)

# get all unique sets
values = unique(unname(unlist(mc, recursive = FALSE)))

# compute value function based on importance
value.function = lapply(values, function(f) {
  calculateValueFunctionImportance(object = mod, data = data, measures = measures,
    n.feat.perm = 5, local = FALSE, features = f)
})
vf = rbindlist(value.function)
vf$features = stri_paste_list(values, ",")

# compute value function based on performance
value.function2 = lapply(values, function(f) {
  calculateValueFunctionPerformance(object = mod, data = data, measures = measures,
    target = "classes", n.feat.perm = 5, local = FALSE, features = f)
})
vf2 = rbindlist(value.function2)
vf2$features = stri_paste_list(values, ",")

#
mc = generateMarginalContribution("x.1", perm)
getShapleyImportance(getMarginalContributionValues(mc, vf), measures = measures)
getShapleyImportance(getMarginalContributionValues(mc, vf2), measures = measures)

s = shapleyImportance(object = mod, data = data, measures = measures,
  target = "classes", n.feat.perm = 5, local = FALSE, features = features)
