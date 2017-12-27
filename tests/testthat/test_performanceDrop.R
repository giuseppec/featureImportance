test_that("performanceDrop with WrappedModel works", {
  learner = makeLearner("classif.rpart", predict.type = "prob")
  task = pid.task
  measures = list(acc, ber, mmce)
  features = getTaskFeatureNames(task)
  resampling = makeResampleInstance(makeResampleDesc("CV", iter = 3), task)

  mod = train(learner, task)
  d = getTaskData(task)
  target = getTaskTargetNames(task)

  res = resample(learner, task, resampling, measures)
  measurePerformance(mod, data = d, features, measures, shuffle = FALSE, local = FALSE)

  #performanceDrop(mod, data = d, features = features, n.feat.perm = 2, measures = measures)
})
