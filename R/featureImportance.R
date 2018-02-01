#' @title Feature Importance
#'
#' @description Measures the feature importance by drop in performance.
#'
#' @template arg_object
#' @template arg_data
#' @param features [list of \code{character}] \cr
#' A list where each element contains the names of at least one feature for which the permutation importance is computed.
#' If a list element contains two or more features, they will be permuted block-wise (without breaking the relationship between those features).
#' @param target [\code{character(1)}] \cr
#' Only needed if \code{object} is not of class \code{WrappedModel} or \code{ResampleResult}.
#' Name of the target feature to be predicted.
#' @template arg_n.feat.perm
#' @template arg_local
#' @template arg_measures
#' @param minimize [named \code{logical}] \cr
#' Only needed if passed \code{measures} is a named list of functions.
#' A named logical of the same length and with the same names as \code{measures} answering the question if smaller values of the measure refer to a better model performance.
#' @template arg_predict.fun
#' @param importance.fun [\code{function}] \cr
#' Function with two arguments \code{function(permuted, unpermuted)} that defines how the permuted and unpermuted predictions are aggregated to a feature importance measure.
#' The function takes the result of \code{\link{measurePerformance}} as input.
#' The default \code{NULL} internally uses \code{unpermuted - permuted} which is the drop in performance.
#' @param ... Not used.
#' @export
featureImportance = function(object, data, features, target = NULL,
  n.feat.perm = 50, local = FALSE, measures, minimize = NULL,
  predict.fun = NULL, importance.fun = NULL, ...) {
  assertDataFrame(data)
  assertList(features, "character")
  assertIntegerish(n.feat.perm, lower = 1L)
  assertFlag(local)
  assertFunction(importance.fun, args = c("permuted", "unpermuted"), null.ok = TRUE)
  UseMethod("featureImportance")
}

#' @export
featureImportance.WrappedModel = function(object, data, features, target = NULL,
  n.feat.perm = 50, local = FALSE, measures, minimize = NULL,
  predict.fun = NULL, importance.fun = NULL, ...) {
  assertNull(target)
  measures = assertMeasure(measures)
  assertNull(minimize)
  assertNull(predict.fun)

  mid = BBmisc::vcapply(measures, function(x) x$id)
  minimize = BBmisc::vlapply(measures, function(x) x$minimize)
  imp = computeFeatureImportance(object, data, features, target, n.feat.perm,
    local, measures, minimize, predict.fun, importance.fun)

  # makeS3Obj(
  #   classes = "featureImportance",
  #   importance = imp,
  #   resample = NULL,
  #   measures = setNames(measures, mid)
  # )
  return(imp)
}

#' @export
featureImportance.ResampleResult = function(object, data, features, target = NULL,
  n.feat.perm = 50, local = FALSE, measures, minimize = NULL,
  predict.fun = NULL, importance.fun = NULL, ...) {

  if (is.null(object$models))
    stop("Use 'models = TRUE' to create the ResampleResult.")

  mid = BBmisc::vcapply(measures, function(x) x$id)
  minimize = BBmisc::vlapply(measures, function(x) x$minimize)
  # for each fold and each feature: permute the feature and measure performance on permuted feature
  ret = lapply(seq_along(object$models), function(i) {
    mod = object$models[[i]]
    train.ind = mod$subset
    test.ind = setdiff(seq_row(data), train.ind)
    if (local) {
      obs.id = test.ind
    } else {
      obs.id = NULL
    }
    computeFeatureImportance(object = mod, data = data[test.ind, ], features = features,
      measures = measures, minimize = minimize, n.feat.perm = n.feat.perm,
      local = local, obs.id = obs.id)
  })

  imp = rbindlist(ret, idcol = "cv.iter")

  # makeS3Obj(
  #   classes = "featureImportance",
  #   importance = imp,
  #   resample = object,
  #   measures = setNames(measures, mid)
  # )
  return(imp)
}

#' @export
featureImportance.default = function(object, data, features, target = NULL,
  n.feat.perm = 50, local = FALSE, measures, minimize = NULL,
  predict.fun = NULL, importance.fun = NULL, ...) {
  assertCharacter(target, null.ok = TRUE)
  assertList(measures, "function", names = "strict")
  assertLogical(minimize, names = "strict", len = length(measures))
  assertSetEqual(names(measures), names(minimize))
  assertFunction(predict.fun, args = c("object", "newdata"), null.ok = TRUE)

  # reorder if not correct
  minimize = minimize[names(measures)]
  imp = computeFeatureImportance(object, data, features, target, n.feat.perm,
    local, measures, minimize, predict.fun, importance.fun)

  # makeS3Obj(
  #   classes = "featureImportance",
  #   importance = imp,
  #   resample = NULL,
  #   measures = measures
  # )
  return(imp)
}

computeFeatureImportance = function(object, data, features, target = NULL,
  n.feat.perm = 50, local = FALSE, measures, minimize = NULL,
  predict.fun = NULL, importance.fun = NULL, obs.id = NULL) {

  if (local & is.null(obs.id))
    obs.id =  BBmisc::seq_row(data)

  # measure performance
  unpermuted.perf = measurePerformance(object, data = data, target = target,
    measures = measures, local = local, predict.fun = predict.fun)

  # FIXME: allow Parallelization
  imp = lapply(seq_len(n.feat.perm), function(i) {
    feat.imp = lapply(features, function(feature) {
      # measure performance when feature is shuffled
      permuted.perf = measurePerformance(object, data = permuteFeature(data, features = feature),
        target = target, measures = measures, local = local, predict.fun = predict.fun)
      # Compare true and shuffled performance
      measureFeatureImportance(permuted.perf, unpermuted.perf, minimize = minimize,
        importance.fun = importance.fun, obs.id = obs.id)
    })
    feat.imp = rbindlist(feat.imp, idcol = "features")
    # replace the feature id with its corresponding feature set
    if (!is.character(features))
      feat.imp$features = stri_paste_list(features[feat.imp$features], sep = ",") #features[feat.imp$features]
    return(feat.imp)
  })
  imp = rbindlist(imp, idcol = "n.feat.perm")
  return(imp)
}
