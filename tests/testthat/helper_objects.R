learner = makeLearner("classif.rpart", predict.type = "prob")
task = pid.task #subsetTask(pid.task, subset = 1:100)
mod = train(learner, task)

features = getTaskFeatureNames(task)
target = getTaskTargetNames(task)
d = getTaskData(task)
