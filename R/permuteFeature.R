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
  replace.id = sample.int(size)
  data[, features] = data[replace.id, features]
  attr(data, "replace.id") = replace.id
  return(data)
}

replaceFeature = function(data, features, replace.id = NULL) {
  #if (length(features) > 1)
  #  warning("Replacing multiple features at once. Not sure if this makes sense.")
  if (any(is.na(features))) {
    if (length(features) == 1) {
      return(data)
    } else {
      features = features[!is.na(features)]
    }
  }
  data[, features] = data[replace.id, features]
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
