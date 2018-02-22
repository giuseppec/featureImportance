#' @export
calculateValueFunctionImportance = function(features, object, data, target = NULL,
  n.feat.perm = 50, measures, predict.fun = NULL) {
  assertCharacter(features)
  features = list(features) # compute importance for whole block

  imp = featureImportance(object = object, data = data, features = features,
    target = target, measures = measures,
    predict.fun = predict.fun, n.feat.perm = n.feat.perm)

  # aggregate importance
  imp.aggr = imp$importance[, lapply(.SD, mean), .SDcols = names(imp$measures), by = "features"]

  return(imp.aggr)
}

#' @export
calculateValueFunctionPerformance = function(features, object, data, target = NULL,
  n.feat.perm = 50, measures, predict.fun = NULL) {
  assertCharacter(features)

  all.feats = setdiff(colnames(data), target)
  # shuffle all features except the ones for which we want to compute the value function
  shuffle.features = setdiff(all.feats, features)
  # compute the value function
  ret = lapply(1:n.feat.perm, function(i) {
    pred = createModelPrediction(object, data = permuteFeature(data, features = shuffle.features),
      target = target, predict.fun = predict.fun)
    perf = measurePerformance(pred, measures = measures)
    if (nrow(perf) != 1)
      stopf("'perf' should contain only one row.")
    #perf = ifelse(minimize, -1, 1)*perf
    perf = cbind(features = stri_paste(features, collapse = ","), perf)
    perf
  })
  ret = rbindlist(ret)

  # we can remove empty.set as value functions are always substracted
  #empty.set = measurePerformance(object, data = permuteFeature(data, features = all.feats),
  #  target = target, measures = measures, predict.fun = predict.fun)
  #return(empty.set - ret) # ret - empty.set when measure should be maximized

  return(ret[, lapply(.SD, mean), by = "features"])
}
