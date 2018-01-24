# measures the drop in performance for a given (true) performance and the performance when a feature was shuffled
# @param perf.shuffled a vector of the performance(s) when a feature was shuffled
# @param perf.true a vector of the true performance(s)
# @param measures the performance measures that have been used: if big values for the measure are better, the drop in performance is true - permuted (negative "drop" values are performance "gains")
measureFeatureImportance = function(perf.shuffled, perf.true, minimize, contrast.fun = NULL, obs.id = NULL) {
  if (is.null(contrast.fun)) {
    contrast.fun = function(perf.shuffled, perf.true, minimize)
      ifelse(minimize, -1, 1) * (perf.true - perf.shuffled)
  }

  fi = lapply(seq_along(minimize), function(i) contrast.fun(perf.shuffled[,i], perf.true[,i], minimize[i]))
  fi = setNames(fi, colnames(perf.true))
  fi = as.data.frame(fi, stringsAsFactors = FALSE)
  fi$obs = obs.id

  return(fi)
}
