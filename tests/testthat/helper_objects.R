learner = makeLearner("classif.rpart", predict.type = "prob")
task = pid.task
mod = train(learner, task)

measures = list(acc, ber, mmce)
mid = BBmisc::vcapply(measures, function(x) x$id)
minimize = !BBmisc::vlapply(measures, function(x) x$minimize)

features = getTaskFeatureNames(task)
target = getTaskTargetNames(task)
d = getTaskData(task)
