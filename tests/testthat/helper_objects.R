set.seed(123)
#options(mlr.debug.seed = 123L)
configureMlr(show.info = FALSE, show.learner.output = FALSE)

# learner and task
learner = mlr::makeLearner("classif.rpart", predict.type = "prob")
predict.fun = function(object, newdata) predict(object, newdata, type = "class")
task = mlr::subsetTask(pid.task, subset = 1:100)

# Extract task infos
features = mlr::getTaskFeatureNames(task)[1:3]
target = mlr::getTaskTargetNames(task)
d = mlr::getTaskData(task)

# Define mlr measure
measures = list(mlr::acc, mlr::mmce)
mid = BBmisc::vcapply(measures, function(x) x$id)

# Define equivalent measures using functions
measures.fun = list(acc = mlr::measureACC, mmce = mlr::measureMMCE)
#minimize = c(acc = FALSE, mmce = TRUE)
mid.fun = names(measures.fun)

# Define repetitions for permutations
n.feat.perm = 2

# Define resampling strategy
resampling = list(
  mlr::makeResampleInstance(mlr::makeResampleDesc("CV", iters = 2), task),
  mlr::makeResampleInstance(mlr::makeResampleDesc("Bootstrap", iters = 2), task),
  mlr::makeResampleInstance(mlr::makeResampleDesc("RepCV", folds = 2, reps = 2), task)
)

# Fit model and ResampleResult
mod = mlr::train(learner, task)
res.list = lapply(resampling, function(rin) mlr::resample(learner, task, rin, measures, models = TRUE, keep.pred = TRUE))
