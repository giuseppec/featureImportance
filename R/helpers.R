assertMeasure = function(measures) {
  if (inherits(measures, "Measure"))
    measures = list(measures)
  assertList(measures, "Measure")
  mid = BBmisc::vcapply(measures, function(x) x$id)
  measures = setNames(measures, mid)
  return(measures)
}

assertResampleResultData = function(object, data, target) {
  td = getTaskDesc(object)
  ts = getTaskSize(td)
  tn = getTaskTargetNames(td)

  # check ResampleResult
  if (is.null(object$models))
    stop("Use 'models = TRUE' to create the ResampleResult.")
  features = object$models[[1]]$features

  # check if target equals target from td
  assertSubset(target, choices = tn, empty.ok = TRUE)

  # some checks for the data ()
  if (ts != nrow(data) | any(tn %nin% colnames(data))) {
    warningf("Use the same data that created the ResampleResult.")
    assertDataFrame(data, nrows = ts)
    assertSubset(features, colnames(data))
  }
}

# guessPerformanceMeasureProperties = function(data, target, pred = NULL) {
#   y = data[[target]]
#   # guess task from target
#   if (is.factor(y)) {
#     properties = "classif"
#     if (nlevels(y) > 2) {
#       properties = c(properties, "classif.multi")
#     }
#   } else {
#     properties = "regr"
#   }
#   # guess pred
#   if (!is.null(pred)) {
#     if (is.matrix(pred) | (!is.factor(pred) & is.factor(y)))  {
#       properties = c(properties, "req.prob")
#     }
#   }
#   return(properties)
# }

# measures the drop in performance for a given (true) performance and the performance when a feature was shuffled
# @param permuted.perf a vector of the performance(s) when a feature was shuffled
# @param unpermuted.perf a vector of the true performance(s)
# @param measures the performance measures that have been used: if big values for the measure are better, the drop in performance is true - permuted (negative "drop" values are performance "gains")
measureFeatureImportance = function(permuted.perf, unpermuted.perf, importance.fun = NULL) {
  mid = setdiff(colnames(permuted.perf), c("row.id", "cv.iter"))

  if (is.null(importance.fun)) {
    importance.fun = function(permuted, unpermuted)
      (unpermuted - permuted)
  }

  fi = lapply(mid, function(i)
    importance.fun(permuted.perf[[i]], unpermuted.perf[[i]]))

  fi = setnames(as.data.table(fi), mid)

  if ("row.id" %in% colnames(permuted.perf))
    fi = cbind("row.id" = permuted.perf$row.id, fi)
  return(fi)
}

# @param data the dataset
# @param features features to be permuted (block-wise)
# @param keep.fixed which column should be kept fixed?
permuteFeature = function(data, features) {
  #assertDataFrame(data)
  #assertSubset(features, colnames(data))
  # FIXME: do we want to permute the whole block-matrix of features or permute each single feature separately? Here we permute the block-matrix containing all features in 'features'.
  if (any(is.na(features))) {
    if (length(features) == 1) {
      return(data)
    } else {
      features = features[!is.na(features)]
    }
  }
  # dim might be faster https://statisfaction.wordpress.com/2017/12/10/nrow-references-and-copies/
  size = nrow(data)
  data[, features] = data[sample.int(size), features]
  return(data)
}

# permuteFeature2 = function(data, feature) {
#   assertDataFrame(data)
#   assertSubset(feature, colnames(data))
#   col.ind = which(colnames(data) == feature)
#   replace(data, list = col.ind, values = sample(data[, feature]))
# }
#
# permuteFeature3 = function(data, feature) {
#   assertDataFrame(data)
#   assertSubset(feature, colnames(data))
#   idx = sample(BBmisc::seq_row(data))
#   col.ind = which(colnames(data) %in% feature)
#   replace(data, list = col.ind, values = data[idx, feature])
# }
