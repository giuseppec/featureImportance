# measures the drop in performance for a given (true) performance and the performance when a feature was shuffled
# @param permuted.perf a vector of the performance(s) when a feature was shuffled
# @param unpermuted.perf a vector of the true performance(s)
# @param measures the performance measures that have been used: if big values for the measure are better, the drop in performance is true - permuted (negative "drop" values are performance "gains")
measureFeatureImportance = function(permuted.perf, unpermuted.perf, minimize,
  importance.fun = NULL) {
  #assertLogical(minimize, len = ncol(permuted.perf))
  mid = setdiff(colnames(permuted.perf), c("row.id", "cv.iter"))

  if (is.null(importance.fun)) {
    importance.fun = function(permuted, unpermuted, minimize)
      ifelse(minimize, -1, 1) * (unpermuted - permuted)
  }

  fi = lapply(mid, function(i)
    importance.fun(permuted.perf[[i]], unpermuted.perf[[i]], minimize[i]))

  fi = setnames(as.data.table(fi), mid)

  if ("row.id" %in% colnames(permuted.perf))
    fi = cbind("row.id" = permuted.perf$row.id, fi)
  return(fi)
}
