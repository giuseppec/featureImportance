#' @title Feature Importance
#'
#' @description Measures the feature importance by drop in performance.
#'
#' @param object [\code{\link[mlr]{WrappedModel}} | \code{\link[mlr]{ResampleResult}}] \cr
#' Currently either the trained \code{\link[mlr]{WrappedModel}} or a \code{\link[mlr]{ResampleResult}}.
#' @param data [\code{data.frame}] \cr
#' The data whose features will be shuffled in order to measure the performance drop.
#' If \code{object} is of class \cr
#' \code{ResampleResult}, you should use the corresponding data on which the resampling was performed \cr
#' \code{WrappedModel}, you should use some independent test data that was not used to fit the model (although you could also use the train data here).
#' @param features [\code{character}] \cr
#' A vector of features for which the importance should be computed.
#' @param measures [list of \code{\link[mlr]{Measure}} | list of \code{function(truth, response)}] \cr
#' Performance measure(s) used to measure the drop in performance.
#' @param n.feat.perm [\code{numeric(1)}] \cr
#' The number of Monte-Carlo iterations, e.g. number of permutations of the feature(s) to compute the feature importance.
#' The default is 10.
#' @param local [\code{logical(1)}] \cr
#' Should observation-wise importance be computed?
#' Note that not all measures support this (e.g. one can not compute the AUC for one observation).
#' The default is FALSE.
#' @param ... Not used.
#' @export
featureImportance = function(object, data, target = NULL, measures, minimize = NULL,
  features, n.feat.perm = 10, local = FALSE, predict.fun = NULL, contrast.fun = NULL, ...) {
  UseMethod("featureImportance")
}

#' @export
featureImportance.WrappedModel = function(object, data, target = NULL, measures, minimize = NULL,
  features, n.feat.perm = 10, local = FALSE, predict.fun = NULL, contrast.fun = NULL, ...) {
  #assertClass(object, "WrappedModel")
  assertDataFrame(data)
  assertCharacter(target, null.ok = TRUE)
  # if (inherits(measures, "Measure"))
  #   measures = list(measures)
  assertList(measures, "Measure")
  assertNull(minimize)
  #assertLogical(minimize, null.ok = TRUE)
  # if (!is.list(features))
  #   features = list(features)
  assertList(features, "character")
  assertIntegerish(n.feat.perm, lower = 1)
  assertFlag(local)

  args = list(...)

  if (local)
    obs.id = args$obs.id else
      obs.id = NULL

  if (is.null(minimize))
    minimize = BBmisc::vlapply(measures, function(x) x$minimize)
  perf.true = measurePerformance(object, data = data, target = target, measures = measures, local = local, predict.fun = predict.fun)

  drop = lapply(features, function(feature) {
    perf.drop = lapply(seq_len(n.feat.perm), function(i) {
      # measure performance when feature is shuffled
      perf.shuffled = measurePerformance(object, data = permuteFeature(data, features = feature),
        target = target, measures = measures, local = local, predict.fun = predict.fun)
      # measure performance drop by comparing with true performance
      measureFeatureImportance(perf.shuffled, perf.true, minimize = minimize, obs.id = obs.id)
    })
    data.table::rbindlist(perf.drop, idcol = "n.feat.perm")
  })
  ret = data.table::rbindlist(setNames(drop, features), idcol = "features")
  #ret = data.table::rbindlist(drop)
  #dt.ret = data.table(features = features)
  #ret = cbind(dt.ret, ret)
  return(ret)
}

#' @export
featureImportance.ResampleResult = function(object, data, target = NULL, measures, minimize = NULL,
  features, n.feat.perm = 10, local = FALSE, predict.fun = NULL, contrast.fun = NULL, ...) {
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
featureImportance.default = function(object, data, target = NULL, measures, minimize = NULL,
  features, n.feat.perm = 10, local = FALSE, predict.fun = NULL, contrast.fun = NULL, ...) {
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

  if (is.null(minimize))
    minimize = !BBmisc::vlapply(measures, function(x) x$minimize)
  perf.true = measurePerformance(object, data = data, target = target, measures = measures, local = local, predict.fun = predict.fun)

  drop = lapply(features, function(feature) {
    perf.drop = lapply(seq_len(n.feat.perm), function(i) {
      # measure performance when feature is shuffled
      perf.shuffled = measurePerformance(object, data = permuteFeature(data, features = feature),
        target = target, measures = measures, local = local, predict.fun = predict.fun)
      # measure performance drop by comparing with true performance
      measureFeatureImportance(perf.shuffled, perf.true, minimize = minimize, obs.id = obs.id)
    })
    data.table::rbindlist(perf.drop, idcol = "n.feat.perm")
  })
  ret = data.table::rbindlist(setNames(drop, features), idcol = "features")
  #ret = data.table::rbindlist(drop)
  #dt.ret = data.table(features = features)
  #ret = cbind(dt.ret, ret)
  return(ret)
}
