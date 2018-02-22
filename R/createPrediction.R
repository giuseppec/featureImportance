#' @title Create Prediction
#'
#' @description Make a predicition object.
#'
#' @template arg_object
#' @template arg_data
#' @template arg_target
#' @template arg_predict.fun
#' @export
createModelPrediction = function(object, data, target = NULL, predict.fun = NULL) {
  assertDataFrame(data)
  assertCharacter(target, null.ok = TRUE)
  UseMethod("createModelPrediction")
}

#' @export
createModelPrediction.WrappedModel = function(object, data, target = NULL, predict.fun = NULL) {
  target = getTaskTargetNames(getTaskDesc(object))
  predict.fun = function(object, newdata) {
    p = predict(object, newdata = newdata)

    tt = getTaskType(object)
    pt = getLearnerPredictType(object$learner)
    tf = getTaskTargetNames(getTaskDesc(object))
    if (tt == "classif") {
      if (pt == "prob") {
        lvls = object$factor.levels[[tf]]
        ret = as.data.table(getPredictionProbabilities(p, cl = lvls)) # FIXME: is data.frame not matrix
      } else if (pt == "response") {
        ret = getPredictionResponse(p)
      } else {
        stop("Unsupported 'predict.type'.")
      }
    } else if (tt == "regr") {
      ret = getPredictionResponse(p)
    } else {
      stop("Unsupported 'Task type'.")
    }
    return(ret)
  }

  createModelPrediction.default(object, data, target = target, predict.fun = predict.fun)
}

#' @export
createModelPrediction.default = function(object, data, target = NULL, predict.fun = NULL) {
  assertString(target)
  assertFunction(predict.fun, args = c("object", "newdata"), null.ok = TRUE)

  y = data[[target]]
  if (is.null(predict.fun))
    predict.fun = function(object, newdata) predict(object, newdata)

  p = predict.fun(object, newdata = data)
  p = checkPrediction(y, p)

  makeS3Obj("ModelPrediction", y = y, pred = p)
}

# checkPrediction
checkPrediction = function(y, p) {
  UseMethod("checkPrediction")
}

checkPrediction.factor = function(y, p) {
  lvls = levels(y)
  if (is.factor(p)) {
    # predict classes: classes must be subset of levels of y
    assertFactor(p, levels = lvls)
    p = factor(p, levels = lvls)
  } else if (is.matrix(p) | is.data.frame(p) | is.data.table(p)) {
    p = as.data.table(p)
    # predict probabilities: prediction should return matrix of probabilities
    if (length(lvls) == ncol(p)) {
      assertNames(colnames(p), must.include = lvls)
      # FIXME: check if colSums is 1
    }
  } else {
    stopf("'predict.fun' returns an object of class '%s' instead of a named data.frame or matrix of probabilities!", class(p)[1L])
  }
  return(p)
}

checkPrediction.default = function(y, p) {
  assertVector(p)
  p
}

