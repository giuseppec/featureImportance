#' @title Feature Importance
#'
#' @description Measures the feature importance by drop in performance.
#'
#' @template arg_object
#' @param data [\code{data.frame}] \cr
#' The data whose features will be shuffled in order to measure the performance drop.
#' If \code{object} is of class \code{ResampleResult}, you should use the corresponding data on which the whole resampling was performed. In any other case, you might use some independent test data that was not used to fit the model (although you could also use the train data here).
#' @param features [list of \code{character}] \cr
#' A list where each element contains the names of at least one feature for which the permutation importance is computed.
#' If a list element contains two or more features, they will be permuted block-wise (without breaking the relationship between those features).
#' @param target [\code{character(1)}] \cr
#' Only needed if \code{object} is not of class \code{WrappedModel} or \code{ResampleResult}.
#' Name of the target feature to be predicted.
#' @template arg_n.feat.perm
#' @template arg_local
#' @template arg_measures
#' @param minimize [\code{logical(1)}] \cr
#' bla
#' @param ... Not used.
#' @export
featureImportance = function(object, data, features, target = NULL, n.feat.perm = 50,
  local = FALSE, measures, minimize = NULL, predict.fun = NULL, importance.fun = NULL, ...) {
  UseMethod("featureImportance")
}

#' @export
featureImportance.WrappedModel = function(object, data, features, target = NULL, n.feat.perm = 50,
  local = FALSE, measures, minimize = NULL, predict.fun = NULL, importance.fun = NULL, ...) {
  assertDataFrame(data)
  assertList(features, "character")
  assertNull(target)
  #assertCharacter(target, null.ok = TRUE)
  if (inherits(measures, "Measure"))
    measures = list(measures)
  assertList(measures, "Measure")
  assertNull(minimize)
  minimize = BBmisc::vlapply(measures, function(x) x$minimize)
  #assertLogical(minimize, null.ok = TRUE)
  # if (!is.list(features))
  #   features = list(features)
  assertIntegerish(n.feat.perm, lower = 1)
  assertFlag(local)

  args = list(...)

  if (local)
    obs.id = args$obs.id else
      obs.id = NULL

  ret = computeFeatureImportance(object, data, features, target, n.feat.perm,
    local, measures, minimize, predict.fun, importance.fun, obs.id)

  return(ret)
}

#' @export
featureImportance.ResampleResult = function(object, data, features, target = NULL, n.feat.perm = 50,
  local = FALSE, measures, minimize = NULL, predict.fun = NULL, importance.fun = NULL, ...) {
  assertClass(object, "ResampleResult")
  if (is.null(object$models))
    stop("Use 'models = TRUE' to create the ResampleResult.")

  # for each fold and each feature: permute the feature and measure performance on permuted feature
  ret = lapply(seq_along(object$models), function(i) {
    mod = object$models[[i]]
    train.ind = mod$subset
    test.ind = setdiff(seq_row(data), train.ind)
    featureImportance(object = mod, data = data[test.ind, ], features = features,
      measures = measures, n.feat.perm = n.feat.perm, local = local,
      obs.id = test.ind)
  })

  data.table::rbindlist(ret, idcol = "cv.iter")
}

#' @export
featureImportance.default = function(object, data, features, target = NULL, n.feat.perm = 50,
  local = FALSE, measures, minimize = NULL, predict.fun = NULL, importance.fun = NULL, ...) {
  assertDataFrame(data)
  assertCharacter(target, null.ok = TRUE)
  assertList(measures, "function", names = "strict")
  #lapply(measures, assertFunction, args = c("truth", "response"))
  assertLogical(minimize, names = "strict", len = length(measures))
  if (!is.list(features))
    features = list(features)
  assertList(features, "character")
  assertIntegerish(n.feat.perm, lower = 1)
  assertFlag(local)
  assertFunction(predict.fun, args = c("object", "newdata"), null.ok = TRUE)

  args = list(...)

  if (local)
    obs.id = args$obs.id else
      obs.id = NULL

  ret = computeFeatureImportance(object, data, features, target, n.feat.perm,
    local, measures, minimize, predict.fun, importance.fun, obs.id)

  return(ret)
}

computeFeatureImportance = function(object, data, features, target = NULL, n.feat.perm = 50,
  local = FALSE, measures, minimize = NULL, predict.fun = NULL, importance.fun = NULL, obs.id = NULL) {

  perf.true = measurePerformance(object, data = data, target = target,
    measures = measures, local = local, predict.fun = predict.fun)

  # FIXME: allow Parallelization
  imp = lapply(seq_len(n.feat.perm), function(i) {
    feat.imp = lapply(features, function(feature) {
      # measure performance when feature is shuffled
      perf.shuffled = measurePerformance(object, data = permuteFeature(data, features = feature),
        target = target, measures = measures, local = local, predict.fun = predict.fun)
      # Compare true and shuffled performance
      measureFeatureImportance(perf.shuffled, perf.true, minimize = minimize,
        importance.fun = importance.fun, obs.id = obs.id)
    })
    feat.imp = rbindlist(feat.imp)
    feat.imp = cbind(data.table(features = features), feat.imp)
    return(feat.imp)
  })
  data.table::rbindlist(imp, idcol = "n.feat.perm")
}

# computeFeatureImportance2 = function(object, data, features, target = NULL, n.feat.perm = 50,
#   local = FALSE, measures, minimize = NULL, predict.fun = NULL, importance.fun = NULL, obs.id = NULL) {
#
#   perf.true = measurePerformance(object, data = data, target = target,
#     measures = measures, local = local, predict.fun = predict.fun)
#
#   drop = lapply(features, function(feature) {
#     perf.drop = lapply(seq_len(n.feat.perm), function(i) {
#       # measure performance when feature is shuffled
#       perf.shuffled = measurePerformance(object, data = permuteFeature(data, features = feature),
#         target = target, measures = measures, local = local, predict.fun = predict.fun)
#       # measure performance drop by comparing with true performance
#       measureFeatureImportance(perf.shuffled, perf.true, minimize = minimize, obs.id = obs.id)
#     })
#     data.table::rbindlist(perf.drop, idcol = "n.feat.perm")
#   })
#   ret = data.table::rbindlist(setNames(drop, features), idcol = "features")
#   #ret = data.table::rbindlist(drop)
#   #dt.ret = data.table(features = features)
#   #ret = cbind(dt.ret, ret)
#   return(ret)
# }
