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

# @param data the dataset
# @param features features to be permuted (block-wise)
# @param keep.fixed which column should be kept fixed?
permuteFeature = function(data, features, keep.fixed = NULL) {
  #assertDataFrame(data)
  #assertSubset(features, colnames(data))
  # FIXME: do we want to permute the whole block-matrix of features or permute each single feature separately? Here we permute the block-matrix containing all features in 'features'.
  if (length(features) == 1) {
    if (is.na(features))
      return(data)
    data[, features] = sample(data[, features])
  } else {
    features = features[!is.na(features)]
    idx = sample(seq_row(data))
    data[, features] = data[idx, features]
  }
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
#   idx = sample(seq_row(data))
#   col.ind = which(colnames(data) %in% feature)
#   replace(data, list = col.ind, values = data[idx, feature])
# }
