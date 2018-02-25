fixupPrediction = function(p, y) {
  UseMethod("checkPrediction")
}

fixupPrediction.factor = function(p, y) {
  assertFactor(y)
  # prediction should have same levels as
  lvls = levels(y)
  assertFactor(p, levels = lvls)
  factor(p, levels = lvls)
}

fixupPrediction.default = function(p, y) {
  assertVector(p)
  p
}

fixupPrediction.matrix = function(p, y) {
  assertClass(y, "factor")
  lvls = levels(y)
  # predict probabilities: prediction should return matrix of probabilities
  if (length(lvls) == ncol(p)) {
    assertNames(colnames(p), must.include = lvls)
  } else {
    stopf("'predict.fun' returns an object of class '%s' instead of a named matrix of probabilities!", class(p)[1L])
  }
}

fixupPrediction.data.frame = function(p, y) {

}

fixupPrediction.data.table = function(p, y) {

}
