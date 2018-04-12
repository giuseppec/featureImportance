#' Measures the Feature Importance
#' Measures the drop in performance between permuted and unpermuted features w.r.t. a function.
#' @param permuted.perf a vector of the performance(s) when a feature was shuffled
#' @param unpermuted.perf a vector of the true performance(s)
#' @param measures the performance measures that have been used: if big values for the measure are better, the drop in performance is true - permuted (negative "drop" values are performance "gains")
#' @export
measureFeatureImportance = function(permuted.perf, unpermuted.perf, importance.fun = NULL) {
  mid = setdiff(colnames(permuted.perf), c("row.id", "cv.iter"))

  if (is.null(importance.fun)) {
    importance.fun = function(permuted, unpermuted)
      (permuted - unpermuted)
  }

  fi = lapply(mid, function(i)
    importance.fun(permuted.perf[[i]], unpermuted.perf[[i]]))

  fi = setnames(as.data.table(fi), mid)

  if ("row.id" %in% colnames(permuted.perf))
    fi = cbind("row.id" = permuted.perf$row.id, fi)
  return(fi)
}
