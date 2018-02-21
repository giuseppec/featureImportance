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
measurePerformance = function(prediction, measures, local = FALSE) {
  assertClass(prediction, "Prediction")
  assertList(measures, "function", names = "strict")
  assertLogical(local)

  p = prediction$pred
  y = prediction$y

  if (local) {
    perf = lapply(measures, function(measures.fun) {
      vnapply(seq_along(y), function(i) {
        measures.fun(y = y[i], pred = p[i])
      })
    })
    perf = c(list(row.id = seq_along(y)), perf)
  } else {
    perf = lapply(measures, function(measures.fun) {
      measures.fun(y = y, pred = p)
    })
  }
  perf = as.data.table(perf)
  return(perf)
}
