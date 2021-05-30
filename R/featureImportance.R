#' @title Feature Importance
#'
#' @description Measures the feature importance by drop in performance.
#'
#' @inheritParams measurePerformance
#' @param features [list of \code{character}] \cr
#' A list where each element contains the names of at least one feature for which the permutation importance should be computed.
#' If a list element contains two or more features, they will be permuted block-wise (without breaking the relationship between those features).
#' @template arg_n.feat.perm
#' @param replace.ids [\code{numeric}] \cr
#' Vector of observation IDs from 'data'.
#' If NULL then permuting the features is used to compute the importance, otherwise the feature values of the IDs are used.
#' Default is NULL.
#' @template arg_importance.fun
#' @param ... Not used.
#' @export
featureImportance = function(object, data, features = NULL, target = NULL,
  n.feat.perm = 50, replace.ids = NULL, local = FALSE, measures, predict.fun = NULL,
  importance.fun = function(permuted, unpermuted) return(unpermuted - permuted),
  ...) {

  assertDataFrame(data)
  assertList(features, "character", null.ok = TRUE)
  assertIntegerish(n.feat.perm, lower = 1L)
  assertIntegerish(replace.ids, lower = 1L, upper = nrow(data), null.ok = TRUE)
  assertFlag(local)
  assertFunction(importance.fun, args = c("permuted", "unpermuted"), null.ok = TRUE)
  UseMethod("featureImportance")
}

#' @export
featureImportance.WrappedModel = function(object, data, features = NULL, target = NULL,
  n.feat.perm = 50, replace.ids = NULL, local = FALSE, measures = mlr::getDefaultMeasure(object$task.desc),
  predict.fun = NULL, importance.fun = NULL, ...) {

  assertSubset(target, choices = mlr::getTaskTargetNames(mlr::getTaskDesc(object)), empty.ok = TRUE)
  measures = assertMeasure(measures)
  assertNull(predict.fun)

  # set defaults
  if (is.null(target))
    target = object$task.desc$target
  if (is.null(features))
    features = as.list(object$features)

  computeFeatureImportance(object = object, data = data, features = features, target = target,
    n.feat.perm = n.feat.perm, replace.ids = replace.ids, local = local, measures = measures,
    predict.fun = predict.fun, importance.fun = importance.fun)
}

#' @export
featureImportance.ResampleResult = function(object, data, features = NULL, target = NULL,
  n.feat.perm = 50, replace.ids = NULL, local = FALSE, measures = mlr::getDefaultMeasure(object$task.desc),
  predict.fun = NULL, importance.fun = NULL, ...) {

  assertResampleResultData(object, data, target)
  measures = assertMeasure(measures)

  # set defaults
  if (is.null(target))
    target = mlr::getTaskTargetNames(mlr::getTaskDesc(object))
  if (is.null(features))
    features = as.list(object$models[[1]]$features)

  # for each fold and each feature: permute the feature and measure performance on permuted feature
  computeFeatureImportance(object = object, data = data, features = features, target = target,
    n.feat.perm = n.feat.perm, replace.ids = replace.ids, local = local, measures = measures,
    predict.fun = predict.fun, importance.fun = importance.fun)
}

#' @export
featureImportance.default = function(object, data, features = NULL, target = NULL,
  n.feat.perm = 50, replace.ids = NULL, local = FALSE, measures, predict.fun = NULL, importance.fun = NULL, ...) {

  assertSubset(target, colnames(data), empty.ok = FALSE)
  assertList(measures, "function", names = "strict")
  assertFunction(predict.fun, args = c("object", "newdata"), null.ok = TRUE)

  # set defaults
  if (is.null(features))
    features = as.list(setdiff(colnames(data), target))

  computeFeatureImportance(object = object, data = data, features = features, target = target,
    n.feat.perm = n.feat.perm, replace.ids = replace.ids, local = local, measures = measures,
    predict.fun = predict.fun, importance.fun = importance.fun)
}

computeFeatureImportance = function(object, data, features, target = NULL,
  n.feat.perm = 50, replace.ids = NULL, local = FALSE, measures,
  predict.fun = NULL, importance.fun = NULL) {

  # measure performance
  unpermuted.perf = measurePerformance(object, data = data, target = target,
    measures = measures, local = local, predict.fun = predict.fun)

  idcol = "n.feat.perm"
  if (!is.null(replace.ids)) {
    iterate = replace.ids
    method = "replace"
  } else {
    iterate = seq_len(n.feat.perm)
    method = "permute"
  }

  fnames = stri_paste_list(features, sep = ",")

  imp = lapply(iterate, function(i) {
    # compute importance for each feature
    feat.imp = setNames(lapply(features, function(feature) {
    # FIXME: split function into: Intervention, Prediction, Performance, Aggregation (if local = FALSE or Resampling)
    computeFeatureImportanceIteration(object = object, i = i, method = method,
      unpermuted.perf = unpermuted.perf, data = data, feature = feature, target = target,
      measures = measures, local = local, predict.fun = predict.fun, importance.fun = importance.fun)
    }), fnames)
    return(rbindlist(feat.imp, idcol = "features", fill = TRUE))
  })
  imp = rbindlist(imp, idcol = idcol)

  # replace the feature column (which is a vector of id) with its corresponding feature sets
  # if (!is.character(imp$features)) {
  #   featnames = stri_paste_list(features, sep = ",")
  #   imp$features = featnames[imp$features]
  # }

  makeS3Obj(
    classes = "featureImportance",
    local = local,
    method = method,
    importance = imp,
    resample = if (inherits(object, "ResampleResult")) object else NULL,
    measures = measures
  )
}

computeFeatureImportanceIteration = function(object, i, method, feature, unpermuted.perf,
  data, target, measures, local, predict.fun, importance.fun) {
  UseMethod("computeFeatureImportanceIteration")
}

computeFeatureImportanceIteration.default = function(object, i, method, feature, unpermuted.perf,
  data, target, measures, local, predict.fun, importance.fun) {
  # permute feature
  if (method == "permute") {
    data.perm = permuteFeature(data, features = feature)
    replace.id = if (local) attr(data.perm, "replace.id") else list(attr(data.perm, "replace.id"))
  } else {
    data.perm = replaceFeature(data, features = feature, replace.id = i)
    replace.id = if (local) rep(i, nrow(data)) else i
  }
  if (local) {
    feature.value = type.convert(data.perm[ , feature])
  } else {
    if (method == "permute" | any(is.na(feature)))
      feature.value = NULL else
        feature.value = unique(type.convert(data.perm[ , feature]))
  }
  # measure performance when feature is shuffled
  permuted.perf = measurePerformance(object, data = data.perm, target = target,
    measures = measures, local = local, predict.fun = predict.fun)
  # Compare true and shuffled performance
  ret = measureFeatureImportance(permuted.perf, unpermuted.perf, importance.fun = importance.fun)
  if (!is.null(feature.value))
    ret = cbind(ret, feature.value = feature.value)
  ret[["replace.id"]] = replace.id
  return(ret)
}

computeFeatureImportanceIteration.ResampleResult = function(object, i, method, feature, unpermuted.perf,
  data, target, measures, local, predict.fun, importance.fun) {
  mid = names(measures)

  imp = lapply(seq_along(object$models), function(i) {
    mod = object$models[[i]]
    train.ind = mod$subset
    test.ind = setdiff(BBmisc::seq_row(data), train.ind)
    test = data[test.ind,]

    # measure performance
    unpermuted.perf = measurePerformance(object = mod, data = test, target = target,
      measures = measures, local = local, predict.fun = predict.fun)

    ret = computeFeatureImportanceIteration(object = mod, i = i, method = method, feature = feature,
      unpermuted.perf = unpermuted.perf, data = test, target = target, measures = measures,
      local = local, predict.fun = predict.fun, importance.fun = importance.fun)
    if (local)
      ret$row.id = test.ind
    return(ret)
  })
  imp = rbindlist(imp, idcol = "cv.iter")

  if (local) {
    if (length(feature) == 1 & object$pred$instance$desc$id == "cross-validation") {
      fv = colnames(imp)[grepl("feature.value", colnames(imp))]
      by = c("row.id", "replace.id", fv)
    } else {
      by = "row.id"
    }
    # if (length(feature) == 1) {
    #   by = c("row.id", "replace.id")
    # } else {
    #   fv = colnames(imp)[grepl("feature.value", colnames(imp))]
    #   by = c("row.id", "replace.id", fv) #setdiff(colnames(imp), c(mid, "cv.iter"))
    # }
    ret = setkey(imp[, lapply(.SD, mean), .SDcols = mid, by = by], "row.id")
  } else {
    ret = imp[, lapply(.SD, mean), .SDcols = mid]
  }

  return(ret)
}
