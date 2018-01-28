# measures the drop in performance for a given (true) performance and the performance when a feature was shuffled
# @param permuted.perf a vector of the performance(s) when a feature was shuffled
# @param unpermuted.perf a vector of the true performance(s)
# @param measures the performance measures that have been used: if big values for the measure are better, the drop in performance is true - permuted (negative "drop" values are performance "gains")
measureFeatureImportance = function(permuted.perf, unpermuted.perf, minimize,
  importance.fun = NULL, obs.id = NULL) {
  assertLogical(minimize, len = ncol(permuted.perf))

  if (is.null(importance.fun)) {
    importance.fun = function(permuted, unpermuted, minimize)
      ifelse(minimize, -1, 1) * (unpermuted - permuted)
  }

  if (!is.null(obs.id) & (nrow(permuted.perf) != length(obs.id)))
    stop("'obs.id' has different length")

  fi = lapply(seq_along(minimize), function(i)
    importance.fun(permuted.perf[,i], unpermuted.perf[,i], minimize[i]))
  fi = setNames(fi, colnames(unpermuted.perf))
  #fi = as.data.frame(fi, stringsAsFactors = FALSE)
  #fi$obs = obs.id
  data.table(obs = obs.id, as.data.table(fi))
}
