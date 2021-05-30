#' @title Value Functions
#' @name valueFunction
#' @rdname valueFunction
#' @description You can write your own value function.
#' @inheritParams shapleyImportance
NULL

#' @rdname valueFunction
#' @format none
#' @export
calculateValueFunctionImportance = function(features, object, data, target = NULL,
  n.feat.perm = 50, measures, predict.fun = NULL, local = FALSE) {
  assertCharacter(features)
  features = list(features) # compute importance for whole block

  imp = featureImportance(object = object, data = data, features = features,
    target = target, measures = measures, local = local,
    predict.fun = predict.fun, n.feat.perm = n.feat.perm)
  # for exact computation replace n.feat.perm with , replace.ids = 1:nrow(data))

  mid = names(imp$measures)
  imp = imp$importance

  # aggregate importance
  if (is.null(imp$row.id))
    imp.aggr = imp[, lapply(.SD, mean), .SDcols = mid, by = "features"] else
      imp.aggr = imp[, lapply(.SD, mean), .SDcols = mid, by = c("features", "row.id")]

  return(imp.aggr)
}

#' @rdname valueFunction
#' @format none
#' @export
calculateValueFunctionPerformance = function(features, object, data, target = NULL,
  n.feat.perm = 50, measures, predict.fun = NULL, local = FALSE) {
  assertCharacter(features)

  all.feats = setdiff(colnames(data), target)
  # shuffle all features except the ones for which we want to compute the value function
  shuffle.features = setdiff(all.feats, features)
  # compute the value function
  ret = lapply(1:n.feat.perm, function(i) {
    ret = measurePerformance(object, data = permuteFeature(data, features = shuffle.features),
      target = target, measures = measures, predict.fun = predict.fun, local = local)
    #if (nrow(ret) != 1)
    #  stopf("'ret' should be only one row.")
    #ret = ifelse(minimize, -1, 1)*ret
    cbind(features = stri_paste(features, collapse = ","), ret)
  })
  ret = rbindlist(ret)

  # we can remove empty.set as value functions are always substracted
  #empty.set = measurePerformance(object, data = permuteFeature(data, features = all.feats),
  #  target = target, measures = measures, predict.fun = predict.fun)
  #return(empty.set - ret) # ret - empty.set when measure should be maximized

  return(ret[, lapply(.SD, mean), by = "features"])
}
