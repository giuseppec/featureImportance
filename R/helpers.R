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

assertMeasure = function(measures) {
  if (inherits(measures, "Measure"))
    measures = list(measures)
  assertList(measures, "Measure")
  mid = BBmisc::vcapply(measures, function(x) x$id)
  measures = setNames(measures, mid)
  return(measures)
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
