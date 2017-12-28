# measures the drop in performance for a given (true) performance and the performance when a feature was shuffled
# @param perf.shuffled a vector of the performance(s) when a feature was shuffled
# @param perf.true a vector of the true performance(s)
# @param measures the performance measures that have been used: if big values for the measure are better, the drop in performance is true - permuted (negative "drop" values are performance "gains")
measurePerformanceDrop = function(perf.shuffled, perf.true, measures, minimize) {
  drop = perf.true - perf.shuffled
  sign = ifelse(minimize, 1, -1)
  return(sign*drop)
}

# FIXME: make S3 method and allow other models: add pred.fun, target, measures as functions(truth, response) and if measures should be minimized
# shuffles feature in test data, predicts and measures performance
measurePerformance = function(mod, data, feature, measures, shuffle = FALSE, local = FALSE) {
  #assertClass(mod, "WrappedModel")
  # if (inherits(measures, "Measure"))
  #     measures = list(measures)
  # m.names = vcapply(measures, function(x) x$id)
  if (shuffle)
    data = permuteFeature(data, feature)
  p = predict(mod, newdata = data)
  if (local) {
    # FIXME: not all measures can handle "local" importance, e.g. auc does not work. We should capture this here.
    p2 = splitPrediction(p, seq_row(p$data))
    perf = lapply(seq_along(p2), function(i) {
      c("obs" = i, mlr::performance(p2[[i]], measures))
    })
    #perf = lapply(seq_row(p$data), function(i) {
    #  p$data = p$data[i, ]
    #  c("obs" = i, mlr::performance(p, measures))
    #})
    perf = as.data.frame(transpose(perf), col.names = names(perf[[1]]), stringsAsFactors = FALSE)
  } else {
    perf = as.data.frame(t(mlr::performance(p, measures)), stringsAsFactors = FALSE)
  }
  return(perf)
}

# Split prediction w.r.t. vector f -> Creates as many prediction objects as unique values in f, see also ?split
splitPrediction = function(p, f) {
  pred.data = split(p$data, f)
  p2 = makeS3Obj(class(p), predict.type = p$predict.type, data = NULL, threshold = p$threshold,
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
