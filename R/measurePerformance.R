#' @title Measure Performance
#'
#' @description Measures the performance on passed data.
#'
#' @template arg_object
#' @template arg_data
#' @template arg_target
#' @template arg_measures
#' @template arg_local
#' @template arg_predict.fun
#' @export
measurePerformance = function(object, data, target = NULL,
  measures, local = FALSE, predict.fun = NULL) {
  assertDataFrame(data)
  assertCharacter(target, null.ok = TRUE)
  assertLogical(local)
  UseMethod("measurePerformance")
}

#' @export
measurePerformance.ResampleResult = function(object, data, target = NULL,
  measures, local = FALSE, predict.fun = NULL) {
  assertResampleResultData(object, data, target)
  measures = assertMeasure(measures)
  mid = names(measures)
  if (is.null(target))
    target = getTaskTargetNames(getTaskDesc(object))

  perf = lapply(seq_along(object$models), function(i) {
    mod = object$models[[i]]
    train.ind = mod$subset
    test.ind = setdiff(BBmisc::seq_row(data), train.ind)
    ret = measurePerformance(mod, data = data[test.ind, ], target = target,
      measures = measures, local = local, predict.fun = predict.fun)
    if (local)
      ret$row.id = test.ind
    ret
  })

  perf = rbindlist(perf, idcol = "cv.iter")

  # aggregate results across cv.iter
  if (local)
    perf = setkey(perf[, lapply(.SD, mean), .SDcols = mid, by = "row.id"], "row.id") else
      perf = perf[, lapply(.SD, mean), .SDcols = mid]

  return(perf)
}

#' @export
measurePerformance.WrappedModel = function(object, data, target = NULL,
  measures, local = FALSE, predict.fun = NULL) {
  measures = assertMeasure(measures)

  p = predict(object, newdata = data)

  if (local) {
    # FIXME: not all measures can handle "local" importance, e.g. auc does not work.
    # We should capture this here.
    p2 = splitPrediction(p, BBmisc::seq_row(p$data))
    perf = lapply(seq_along(p2), function(i) {
      # this is slower: p = predict(object, newdata = data, subset = i)
      mlr::performance(p2[[i]], measures)
    })
    cn = names(perf[[1]])
    perf = setnames(as.data.table(transpose(perf)), cn)
    perf$row.id = BBmisc::seq_row(data)
    cn = c("row.id", cn)
    perf = setcolorder(perf, cn)
  } else {
    perf = as.data.table(t(mlr::performance(p, measures)))
  }
  return(perf)
}

# @export
measurePerformance.default = function(object, data, target = NULL,
  measures, local = FALSE, predict.fun = NULL) {
  #assertSubset(target, colnames(data), empty.ok = FALSE)
  assertString(target)
  assertList(measures, "function", names = "strict")
  assertFunction(predict.fun, args = c("object", "newdata"), null.ok = TRUE)

  truth = data[[target]]
  if (is.null(predict.fun))
    predict.fun = function(object, newdata) predict(object, newdata)

  p = predict.fun(object, newdata = data)
  p = checkPrediction(truth, p)

  if (local) {
    perf = lapply(measures, function(measures.fun) {
      vnapply(seq_along(p), function(i) {
        measures.fun(truth = truth[i], response = p[i])
      })
    })
    perf = c(list(row.id = BBmisc::seq_row(data)), perf)
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
