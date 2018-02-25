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

checkPrediction = function(y, p) {
  UseMethod("checkPrediction")
}

checkPrediction.factor = function(y, p) {
  lvls = levels(y)
  if (is.factor(p)) {
    # predict classes: classes must be subset of levels of y
    assertFactor(p, levels = lvls)
    p = factor(p, levels = lvls)
  } else if (is.matrix(p)) {
    # predict probabilities: prediction should return matrix of probabilities
    if (length(lvls) == ncol(p)) {
      assertNames(colnames(p), must.include = lvls)
    } else {
      stopf("'predict.fun' returns an object of class '%s' instead of a named matrix of probabilities!", class(p)[1L])
    }
  }
  p
}

checkPrediction.character = function(y, p) {
  checkPrediction(as.factor(y), p)
}

checkPrediction.default = function(y, p) {
  assertVector(p)
  p
}

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
permuteFeature = function(data, features, all.permutations = FALSE) {
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

  if (all.permutations) {
    # row.indices = rep(1:size, times = size)
    # replace.indices = rep(1:size, each = size)
    # keep.indices = row.indices != replace.indices
    # data.all = rbindlist(lapply(1:size, function(i) data))
    # reord = function(x, ind) x[ind]
    # data.perm = copy(data.all)
    # data.perm[, (features) := lapply(.SD, reord, ind = (replace.indices)), .SDcols = features]
    # data.perm = data.perm[keep.indices, ]
    return(generateAllPermutationData(as.data.table(data), features = features))
  } else {
    data[, features] = data[sample.int(size), features]
    return(data)
  }
}

generateAllPermutationData = function(data, features) {
  assertClass(data, "data.table")
  size = nrow(data)
  row.indices = rep(1:size, times = size)
  replace.indices = rep(1:size, each = size)
  keep.indices = row.indices != replace.indices
  row.indices = row.indices[keep.indices]
  replace.indices = replace.indices[keep.indices]

  cols = colnames(data)
  const.features = setdiff(cols, features)

  data.const = data[row.indices, const.features, with = FALSE]
  data.const = data.const[, (features) := data[replace.indices, features, with = FALSE]]
  as.data.frame(setcolorder(data.const, cols))
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
