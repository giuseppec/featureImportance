#' @title Drop in Performance
#'
#' @description Measures the drop in performance.
#'
#' @template arg_object
#' @param data [\code{data.frame}] \cr
#' The data whose features will be shuffled in order to measure the performance drop.
#' If \code{object} is of class \cr
#' \code{ResampleResult}, you should use the corresponding data on which the resampling was performed \cr
#' \code{WrappedModel}, you should use some independent test data that was not used to fit the model (although you could also use the train data here).
#' @param features [\code{character}] \cr
#' A vector of features for which the importance should be computed.
#' @template arg_measures
#' @template arg_n.feat.perm
#' @template arg_local
#' @param ... Not used.
#' @export
performanceDrop = function(object, data, features, measures, n.feat.perm = 10, local = FALSE, ...) {
  UseMethod("performanceDrop")
}

#' @export
performanceDrop.WrappedModel = function(object, data, features, measures, n.feat.perm = 10, local = FALSE, ...) {
  assertClass(object, "WrappedModel")
  assertDataFrame(data)
  # FIXME: assert features (we should allow list of characters)
  # if (!is.list(features))
  #   features = list(features)
  if (inherits(measures, "Measure"))
    measures = list(measures)
  assertList(measures, "Measure")
  assertIntegerish(n.feat.perm)
  assertFlag(local)

  args = list(...)

  minimize = !BBmisc::vlapply(measures, function(x) x$minimize)
  perf.true = measurePerformance(object, data = data, measures = measures, shuffle = FALSE, local = local)

  drop = lapply(features, function(feature) {
    perf.drop = replicate(n.feat.perm, {
      # measure performance when feature is shuffled
      perf.shuffled = measurePerformance(object, data, feature, measures, shuffle = TRUE, local = local)
      # measure performance drop by comparing with true performance
      measurePerformanceDrop(perf.shuffled, perf.true, minimize, obs.id = args$obs.id)
    }, simplify = FALSE)
    data.table::rbindlist(perf.drop, idcol = "n.feat.perm")
  })
  data.table::rbindlist(setNames(drop, features), idcol = "features")
}

#' @export
performanceDrop.ResampleResult = function(object, data, features, measures, n.feat.perm = 10, local = FALSE, ...) {
  assertClass(object, "ResampleResult")
  if (is.null(object$models))
    stop("Use 'models = TRUE' to create the ResampleResult.")

  # for each fold and each feature: permute the feature and measure performance on permuted feature
  ret = lapply(seq_along(object$models), function(i) {
    mod = object$models[[i]]
    train.ind = mod$subset
    test.ind = setdiff(seq_row(data), train.ind)
    performanceDrop(object = mod, data = data[test.ind, ], features = features,
      measures = measures, n.feat.perm = n.feat.perm, local = local,
      obs.id = test.ind)
  })

  data.table::rbindlist(ret, idcol = "cv.iter")
}
