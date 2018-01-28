# measures the drop in performance for a given (true) performance and the performance when a feature was shuffled
# @param perf.shuffled a vector of the performance(s) when a feature was shuffled
# @param perf.true a vector of the true performance(s)
# @param measures the performance measures that have been used: if big values for the measure are better, the drop in performance is true - permuted (negative "drop" values are performance "gains")
measureFeatureImportance = function(perf.shuffled, perf.true, minimize,
  importance.fun = NULL, obs.id = NULL) {
  assertLogical(minimize, len = ncol(perf.shuffled))

  if (is.null(importance.fun)) {
    importance.fun = function(perf.shuffled, perf.true, minimize)
      ifelse(minimize, -1, 1) * (perf.true - perf.shuffled)
  }

  if (!is.null(obs.id) & (nrow(perf.shuffled) != length(obs.id)))
    stop("'obs.id' has different length")

  fi = lapply(seq_along(minimize), function(i)
    importance.fun(perf.shuffled[,i], perf.true[,i], minimize[i]))
  fi = setNames(fi, colnames(perf.true))
  #fi = as.data.frame(fi, stringsAsFactors = FALSE)
  #fi$obs = obs.id
  data.table(obs = obs.id, as.data.table(fi))
}
