permuteFeature = function(data, features) {
  #assertDataFrame(data)
  #assertSubset(features, colnames(data))
  idx = sample(seq_row(data))
  data[, feature] = data[idx, features]
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
