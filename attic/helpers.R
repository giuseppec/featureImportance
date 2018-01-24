# measures the drop in performance for a given (true) performance and the performance when a feature was shuffled
# @param perf.shuffled a vector of the performance(s) when a feature was shuffled
# @param perf.true a vector of the true performance(s)
# @param measures the performance measures that have been used: if big values for the measure are better, the drop in performance is true - permuted (negative "drop" values are performance "gains")
measurefeatureImportance = function(perf.shuffled, perf.true, minimize, obs.id = NULL) {
  drop = perf.true - perf.shuffled
  sign = ifelse(minimize, 1, -1)
  if (nrow(perf.shuffled) == 1) {
    return(sign*drop)
  } else {
    if (is.null(obs.id))
      obs.id = seq_row(perf.shuffled)
    ret = as.matrix(drop) %*% diag(sign)
    ret = setColNames(as.data.frame(ret, stringsAsFactors = FALSE), colnames(drop))
    ret = cbind("obs" = obs.id, ret)
    return(ret)
  }
}

measurePerformance = function(object, data, target = NULL, measures, minimize = NULL,
  shuffle.features = NULL, local = FALSE, predict.fun = NULL) {
  UseMethod("measurePerformance")
}

# FIXME: make S3 method and allow other models: add pred.fun, target, measures as functions(truth, response) and if measures should be minimized
# shuffles feature in test data, predicts and measures performance
measurePerformance.WrappedModel = function(object, data, target = NULL, measures, minimize = NULL,
  shuffle.features = NULL, local = FALSE, predict.fun = NULL) {
  #assertClass(object, "WrappedModel")
  # if (inherits(measures, "Measure"))
  #     measures = list(measures)
  # m.names = vcapply(measures, function(x) x$id)
  if (!is.null(shuffle.features))
    data = permuteFeature(data, shuffle.features)
  p = predict(object, newdata = data)
  if (local) {
    # FIXME: not all measures can handle "local" importance, e.g. auc does not work. We should capture this here.
    p2 = splitPrediction(p, seq_row(p$data))
    perf = lapply(seq_along(p2), function(i) {
      mlr::performance(p2[[i]], measures)
    })
    perf = as.data.frame(transpose(perf), col.names = names(perf[[1]]), stringsAsFactors = FALSE)
  } else {
    perf = as.data.frame(t(mlr::performance(p, measures)), stringsAsFactors = FALSE)
  }
  return(perf)
}

# FIXME: remove feature and shuffle and add shuffle.features
measurePerformance.default = function(object, data, target, feature = NULL, measures,
  shuffle = FALSE, local = FALSE, predict.fun = NULL) {
  if (is.null(predict.fun))
    predict.fun = function(object, newdata) predict(object, newdata)
  assertFunction(predict.fun, args = c("object", "newdata"), null.ok = TRUE)

  if (shuffle)
    data = permuteFeature(data, feature)
  p = predict.fun(object, newdata = data)

  truth = data[, target]
  if (is.factor(truth)) {
    createTask = makeClassifTask
    if (is.numeric(p)) predict.type = "prob" else predict.type = "response"
  } else {
    createTask = makeRegrTask
    predict.type = "response"
  }
  task = createTask(data = data, target = target)

  p = mlr::makePrediction(getTaskDesc(task), id = row.names(data),
    truth = truth, y = p, row.names = row.names(data),
    predict.type = predict.type, time = NA)

  if (local) {
    # FIXME: not all measures can handle "local" importance, e.g. auc does not work. We should capture this here.
    p2 = splitPrediction(p, seq_row(p$data))
    perf = lapply(seq_along(p2), function(i) {
      mlr::performance(p2[[i]], measures)
    })
    perf = as.data.frame(transpose(perf), col.names = names(perf[[1]]), stringsAsFactors = FALSE)
  } else {
    perf = as.data.frame(t(mlr::performance(p, measures)), stringsAsFactors = FALSE)
  }
  return(perf)
}

# Split prediction w.r.t. vector f -> Creates as many prediction objects as unique values in f, see also ?split
splitPrediction = function(p, f) {
  pred.data = split(p$data, f)
  p2 = BBmisc::makeS3Obj(class(p), predict.type = p$predict.type, data = NULL, threshold = p$threshold,
    task.desc = p$task.desc, time = p$time, error = p$error, dump = p$dump)
  lapply(pred.data, function(x) {
    p2$data = x
    p2
  })
}

# this is faster for small DS
# measurePerformance2 = function(mod, data, feature, measures, shuffle = FALSE, local = FALSE, n.feat.perm = 1) {
#   if (shuffle) {
#     data = replicate(n.feat.perm, {
#       permuteFeature(data, feature)
#     }, simplify = FALSE)
#     data = as.data.frame(data.table::rbindlist(data, idcol = "n.feat.perm"), stringsAsFactors = FALSE)
#   }
#   p = predict(mod, newdata = data)
#   #pred.data = split(p$data, data$n.feat.perm)
#   p2 = splitPrediction(p, data$n.feat.perm)
#
#   if (local) {
#      p2 = splitPrediction(p, seq_row(p$data))
#      perf = lapply(seq_along(p2), function(i) {
#        c("obs" = i, mlr::performance(p2[[i]], measures))
#      })
#   } else {
#     perf = lapply(p2, function(x) {
#       as.data.frame(t(mlr::performance(x, measures)), stringsAsFactors = FALSE)
#     })
#   }
#   return(perf)
# }
#
