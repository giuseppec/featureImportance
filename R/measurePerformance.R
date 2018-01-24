# shuffles feature in data, predicts and measures performance
# @param object the model
# @param data the data
# @param target the target feature, not relevat if object is WrappedModel
# @param measures if object is WrappedModel this should be a list of Measures otherwise a list of functions(truth, response)
# @param shuffle.features character containing all the features that will be (jointly) shuffled
# @param local should observation-wise performances be returned?
# @param predict.fun the prediction function for object (not needed if object is WrappedModel)
measurePerformance = function(object, data, target = NULL, measures,
  local = FALSE, predict.fun = NULL) {
  UseMethod("measurePerformance")
}

measurePerformance.WrappedModel = function(object, data, target = NULL, measures,
  local = FALSE, predict.fun = NULL) {
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

measurePerformance.default = function(object, data, target = NULL, measures,
  local = FALSE, predict.fun = NULL) {
  assertSubset(target, colnames(data), empty.ok = FALSE)
  if (is.null(predict.fun))
    predict.fun = function(object, newdata) predict(object, newdata)

  p = predict.fun(object, newdata = data)
  truth = data[, target]

  if (!(is.vector(p) | is.factor(p)))
    stop("Make sure that 'predict.fun' returns a vector.")

  if (local) {
    # FIXME: not all measures can handle "local" importance, e.g. auc does not work. We should capture this here.
    perf = lapply(measures, function(measures.fun) {
      vnapply(seq_along(p), function(i) {
        measures.fun(truth = truth[i], response = p[i])
      })
    })
  } else {
    perf = lapply(measures, function(measures.fun) {
      measures.fun(truth = truth, response = p)
    })
  }
  perf = setNames(perf, names(measures))
  perf = as.data.frame(perf, stringsAsFactors = FALSE)
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
