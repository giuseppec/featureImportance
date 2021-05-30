assertMeasure = function(measures) {
  if (inherits(measures, "Measure"))
    measures = list(measures)
  assertList(measures, "Measure")
  mid = BBmisc::vcapply(measures, function(x) x$id)
  measures = setNames(measures, mid)
  return(measures)
}

assertResampleResultData = function(object, data, target) {
  td = mlr::getTaskDesc(object)
  ts = mlr::getTaskSize(td)
  tn = mlr::getTaskTargetNames(td)

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
