#' @title Feature Importance
#'
#' @description Measures the feature importance by drop in performance.
#'
#' @inheritParams measurePerformance
#' @param features [list of \code{character}] \cr
#' A list where each element contains the names of at least one feature for which the permutation importance should be computed.
#' If a list element contains two or more features, they will be permuted block-wise (without breaking the relationship between those features).
#' @template arg_n.feat.perm
#' @param minimize [named \code{logical}] \cr
#' Only needed if passed \code{measures} is a named list of functions.
#' A named logical of the same length and with the same names as \code{measures} answering the question if smaller values of the measure refer to a better model performance.
#' @template arg_importance.fun
#' @param ... Not used.
#' @export
featureImportance = function(object, data, features = NULL, target = NULL,
  n.feat.perm = 50, local = FALSE, measures, minimize = NULL, predict.fun = NULL,
  importance.fun = function(permuted, unpermuted, minimize)
    ifelse(minimize, -1, 1) * (unpermuted - permuted),
  ...) {

  assertDataFrame(data)
  assertList(features, "character", null.ok = TRUE)
  assertIntegerish(n.feat.perm, lower = 1L)
  assertFlag(local)
  assertFunction(importance.fun, args = c("permuted", "unpermuted", "minimize"), null.ok = TRUE)
  UseMethod("featureImportance")
}

#' @export
featureImportance.WrappedModel = function(object, data, features = NULL, target = NULL,
  n.feat.perm = 50, local = FALSE, measures = mlr::getDefaultMeasure(object$task.desc),
  minimize = NULL, predict.fun = NULL, importance.fun = NULL, ...) {

  tn = getTaskTargetNames(getTaskDesc(object))
  assertSubset(target, choices = tn, empty.ok = TRUE)
  measures = assertMeasure(measures)
  assertNull(predict.fun)
  assertNull(minimize)

  # set defaults
  minimize = BBmisc::vlapply(measures, function(x) x$minimize)
  if (is.null(target))
    target = object$task.desc$target
  if (is.null(features))
    features = as.list(object$features)

  computeFeatureImportance(object = object, data = data, features = features, target = target,
    n.feat.perm = n.feat.perm, local = local, measures = measures, minimize = minimize,
    predict.fun = predict.fun, importance.fun = importance.fun)
}

#' @export
featureImportance.ResampleResult = function(object, data, features = NULL, target = NULL,
  n.feat.perm = 50, local = FALSE, measures = mlr::getDefaultMeasure(object$task.desc), minimize = NULL,
  predict.fun = NULL, importance.fun = NULL, ...) {

  td = getTaskDesc(object)
  ts = getTaskSize(td)
  tn = getTaskTargetNames(td)

  assertSubset(target, choices = tn, empty.ok = TRUE)
  if (is.null(target))
    target = tn

  measures = assertMeasure(measures)
  minimize = BBmisc::vlapply(measures, function(x) x$minimize)

  # for each fold and each feature: permute the feature and measure performance on permuted feature
  computeFeatureImportance(object = object, data = data, features = features, target = target,
    n.feat.perm = n.feat.perm, local = local, measures = measures, minimize = minimize,
    predict.fun = predict.fun, importance.fun = importance.fun)
}

#' @export
featureImportance.default = function(object, data, features = NULL, target = NULL,
  n.feat.perm = 50, local = FALSE, measures, minimize = NULL,
  predict.fun = NULL, importance.fun = NULL, ...) {

  assertSubset(target, colnames(data), empty.ok = FALSE)
  assertList(measures, "function", names = "strict")
  assertLogical(minimize, names = "strict", len = length(measures))
  assertSetEqual(names(measures), names(minimize))
  assertFunction(predict.fun, args = c("object", "newdata"), null.ok = TRUE)

  if (is.null(features))
    features = as.list(setdiff(colnames(data), target))

  # reorder if not correct
  minimize = minimize[names(measures)]
  computeFeatureImportance(object = object, data = data, features = features, target = target,
    n.feat.perm = n.feat.perm, local = local, measures = measures, minimize = minimize,
    predict.fun = predict.fun, importance.fun = importance.fun)
}

computeFeatureImportance = function(object, data, features, target = NULL,
  n.feat.perm = 50, local = FALSE, measures, minimize = NULL,
  predict.fun = NULL, importance.fun = NULL) {

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
        importance.fun = importance.fun)
    })
    feat.imp = rbindlist(feat.imp, idcol = "features")
    # replace the feature id with its corresponding feature set
    if (!is.character(features))
      feat.imp$features = stri_paste_list(features[feat.imp$features], sep = ",")
    #features[feat.imp$features]
    return(feat.imp)
  })
  imp = rbindlist(imp, idcol = "n.feat.perm")

  makeS3Obj(
    classes = "featureImportance",
    importance = imp,
    resample = if (inherits(object, "ResampleResult")) object else NULL,
    measures = measures
  )
}
