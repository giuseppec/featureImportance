#' @title Feature Importance
#'
#' @description Measures the feature importance by drop in performance.
#'
#' @template arg_object
#' @template arg_data
#' @template arg_target
#' @template arg_measures
#' @template arg_local
#' @template arg_predict.fun
#' @param features [list of \code{character}] \cr
#' A list where each element contains the names of at least one feature for which the permutation importance should be computed.
#' If a list element contains two or more features, they will be permuted block-wise (without breaking the relationship between those features).
#' @template arg_n.feat.perm
#' @template arg_importance.fun
#' @param ... Not used.
#' @export
featureImportance = function(object, data, features = NULL, target = NULL,
  n.feat.perm = 50, local = FALSE, measures, predict.fun = NULL,
  importance.fun = function(permuted, unpermuted) return(unpermuted - permuted),
  ...) {

  assertDataFrame(data)
  assertList(features, "character", null.ok = TRUE)
  assert(checkIntegerish(n.feat.perm, lower = 1L), checkSubset(n.feat.perm, choices = "all.permutations"))
  assertFlag(local)
  assertFunction(importance.fun, args = c("permuted", "unpermuted"), null.ok = TRUE)
  UseMethod("featureImportance")
}

#' @export
featureImportance.WrappedModel = function(object, data, features = NULL, target = NULL,
  n.feat.perm = 50, local = FALSE, measures = mlr::getDefaultMeasure(object$task.desc),
  predict.fun = NULL, importance.fun = NULL, ...) {

  assertSubset(target, choices = getTaskTargetNames(getTaskDesc(object)), empty.ok = TRUE)
  measures = assertMeasure(measures)
  assertNull(predict.fun)

  # set defaults
  if (is.null(target))
    target = object$task.desc$target
  if (is.null(features))
    features = as.list(object$features)

  computeFeatureImportance(object = object, data = data, features = features, target = target,
    n.feat.perm = n.feat.perm, local = local, measures = measures,
    predict.fun = predict.fun, importance.fun = importance.fun)
}

#' @export
featureImportance.ResampleResult = function(object, data, features = NULL, target = NULL,
  n.feat.perm = 50, local = FALSE, measures = mlr::getDefaultMeasure(object$task.desc),
  predict.fun = NULL, importance.fun = NULL, ...) {

  assertResampleResultData(object, data, target)
  measures = assertMeasure(measures)

  # set defaults
  if (is.null(target))
    target = getTaskTargetNames(getTaskDesc(object))

  # for each fold and each feature: permute the feature and measure performance on permuted feature
  computeFeatureImportance(object = object, data = data, features = features, target = target,
    n.feat.perm = n.feat.perm, local = local, measures = measures,
    predict.fun = predict.fun, importance.fun = importance.fun)
}

#' @export
featureImportance.default = function(object, data, features = NULL, target = NULL,
  n.feat.perm = 50, local = FALSE, measures, predict.fun = NULL, importance.fun = NULL, ...) {

  assertSubset(target, colnames(data), empty.ok = FALSE)
  assertList(measures, "function", names = "strict")
  assertFunction(predict.fun, args = c("object", "newdata"), null.ok = TRUE)

  # set defaults
  if (is.null(features))
    features = as.list(setdiff(colnames(data), target))

  computeFeatureImportance(object = object, data = data, features = features, target = target,
    n.feat.perm = n.feat.perm, local = local, measures = measures,
    predict.fun = predict.fun, importance.fun = importance.fun)
}

computeFeatureImportance = function(object, data, features, target = NULL,
  n.feat.perm = 50, local = FALSE, measures,
  predict.fun = NULL, importance.fun = NULL) {

  # measure performance
  unpermuted.perf = measurePerformance(object, data = data, target = target,
    measures = measures, local = local, predict.fun = predict.fun)

  # build arg list
  args = list(unpermuted.perf = unpermuted.perf, object = object, data = data,
    features = features, target = target, measures = measures, local = local,
    predict.fun = predict.fun, importance.fun = importance.fun)

  # Parallelize over n.feat.perm
  if (n.feat.perm == "all.permutations") {
    args$all.permutations = TRUE
    imp = do.call(computeFeatureImportanceIteration, args = args)
  } else {
    imp = parallelMap::parallelMap(computeFeatureImportanceIteration,
      i = seq_len(n.feat.perm), more.args = args)
    imp = rbindlist(imp, idcol = "n.feat.perm")
  }

  # replace the feature column (which is a vector of id) with its corresponding feature sets
  if (!is.character(imp$features))
    imp$features = stri_paste_list(features[imp$features], sep = ",")

  makeS3Obj(
    classes = "featureImportance",
    importance = imp,
    resample = if (inherits(object, "ResampleResult")) object else NULL,
    measures = measures
  )
}

computeFeatureImportanceIteration = function(i, features, unpermuted.perf, object,
  data, target, measures, local, predict.fun, importance.fun, all.permutations = FALSE) {

  # compute importance for each feature
  feat.imp = lapply(features, function(feature) {
    # permute feature
    data.perm = permuteFeature(data, features = feature, all.permutations = all.permutations)
    # measure performance when feature is shuffled
    permuted.perf = measurePerformance(object, data = data.perm, target = target,
      measures = measures, local = local, predict.fun = predict.fun)
    # Compare true and shuffled performance
    measureFeatureImportance(permuted.perf, unpermuted.perf, importance.fun = importance.fun)
  })
  feat.imp = rbindlist(feat.imp, idcol = "features")

  return(feat.imp)
}
