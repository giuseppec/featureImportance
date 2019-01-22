set.seed(123)
#options(mlr.debug.seed = 123L)
configureMlr(show.info = FALSE, show.learner.output = FALSE)

# learner and task
learner = makeLearner("classif.rpart", predict.type = "prob")
predict.fun = function(object, newdata) predict(object, newdata, type = "class")
task = pid.task #subsetTask(pid.task, subset = 1:100)

# Extract task infos
features = getTaskFeatureNames(task)[1:3]
target = getTaskTargetNames(task)
d = getTaskData(task)

# Define mlr measure
measures = list(acc, mmce)
mid = BBmisc::vcapply(measures, function(x) x$id)

# Define equivalent measures using functions
measures.fun = list(acc = mlr::measureACC, mmce = mlr::measureMMCE)
#minimize = c(acc = FALSE, mmce = TRUE)
mid.fun = names(measures.fun)

# Define repetitions for permutations
n.feat.perm = 2

# Define resampling strategy
resampling = list(
  makeResampleInstance(makeResampleDesc("CV", iters = 2), task),
  makeResampleInstance(makeResampleDesc("Bootstrap", iters = 2), task),
  makeResampleInstance(makeResampleDesc("RepCV", folds = 2, reps = 2), task)
)

# Fit model and ResampleResult
mod = train(learner, task)
res.list = lapply(resampling, function(rin) resample(learner, task, rin, measures, models = TRUE, keep.pred = TRUE))
