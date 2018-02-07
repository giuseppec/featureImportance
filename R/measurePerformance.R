#' @title Measure Performance
#'
#' @description Measures the performance on passed data.
#'
#' @param object [any] \cr
#' Either a \code{\link[mlr]{WrappedModel}} or any other trained model (however, the latter is experimental and you need to provide an appropriate \code{predict.fun}).
#' @param data [\code{data.frame}] \cr
#' The data for which the performance will be measured.
#' @param target [\code{character(1)}] \cr
#' Only needed if \code{object} is not of class \code{WrappedModel}.
#' Name of the target feature to be predicted.
#' @template arg_measures
#' @template arg_local
#' @template arg_predict.fun
#' @export
measurePerformance = function(object, data, target = NULL, measures,
  local = FALSE, predict.fun = NULL) {
  UseMethod("measurePerformance")
}

#' @export
measurePerformance.ResampleResult = function(object, data, target = NULL, measures,
  local = FALSE, predict.fun = NULL) {
  mid = BBmisc::vcapply(measures, function(x) x$id)

  # FIXME: local not working here
  assertFALSE(local)

  perf = lapply(seq_along(object$models), function(i) {
    mod = object$models[[i]]
    train.ind = mod$subset
    test.ind = setdiff(seq_row(data), train.ind)
    measurePerformance(mod, data = data[test.ind, ], target = target, measures = measures,
      local = local, predict.fun = predict.fun)
  })

  perf = rbindlist(perf, idcol = "cv.iter")
  perf[, lapply(.SD, mean), .SDcols = mid]
}

#' @export
measurePerformance.WrappedModel = function(object, data, target = NULL, measures,
  local = FALSE, predict.fun = NULL) {
  p = predict(object, newdata = data)
  if (local) {
    # FIXME: not all measures can handle "local" importance, e.g. auc does not work.
    # We should capture this here.
    p2 = splitPrediction(p, seq_row(p$data))
    perf = lapply(seq_along(p2), function(i) {
      # this is slower: p = predict(object, newdata = data, subset = i)
      mlr::performance(p2[[i]], measures)
    })
    perf = setnames(as.data.table(transpose(perf)), names(perf[[1]]))
  } else {
    perf = as.data.table(t(mlr::performance(p, measures)))
  }
  return(perf)
}

#' @export
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
  perf = as.data.table(perf)
  return(perf)
}

# Split prediction w.r.t. vector f -> Creates as many prediction objects as unique values in f, see also ?split
splitPrediction = function(p, f) {
  pred.data = split(p$data, f)
  p2 = BBmisc::makeS3Obj(class(p), predict.type = p$predict.type, data = NULL,
    threshold = p$threshold, task.desc = p$task.desc, time = p$time,
    error = p$error, dump = p$dump)
  lapply(pred.data, function(x) {
    p2$data = x
    p2
  })
}
