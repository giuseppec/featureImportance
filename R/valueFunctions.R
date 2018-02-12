calculateValueFunctionImportance = function(features, object, data, target = NULL,
  measures, n.feat.perm = 50) {
  assertCharacter(features)
  features = list(features) # compute importance for whole block
  measures = assertMeasure(measures)
  #assertNull(target)

  # FIXME: allow measures to be also functions
  mid = names(measures)
  imp = featureImportance(object = object, data = data, features = features,
    measures = measures, n.feat.perm = n.feat.perm)
  # aggregate importance
  imp.aggr = imp$importance[, lapply(.SD, mean), .SDcols = mid, by = "features"]
  return(imp.aggr)
}

calculateValueFunctionPerformance = function(features, object, data, target, measures,
  n.feat.perm = 50, predict.fun = NULL) {
  assertCharacter(features)
  assertSubset(target, colnames(data))
  measures = assertMeasure(measures)

  mid = names(measures)
  minimize = BBmisc::vlapply(measures, function(x) x$minimize)
  all.feats = setdiff(colnames(data), target)
  # shuffle all features except the ones for which we want to compute the value function
  shuffle.features = setdiff(all.feats, features)
  # compute the value function
  ret = lapply(1:n.feat.perm, function(i) {
    ret = measurePerformance(object, data = permuteFeature(data, features = shuffle.features),
      target = target, measures = measures, predict.fun = predict.fun)
    if (nrow(ret) != 1)
      stopf("'ret' should be only one row.")
    ret = ifelse(minimize, -1, 1)*ret
  })
  ret = rbindlist(ret)
  # we can remove empty.set which is always substracted as we use differences of value functions
  #empty.set = measurePerformance(object, data = permuteFeature(data, features = all.feats),
  #  target = target, measures = measures, predict.fun = predict.fun)
  # FIXME: ret - empty.set when measure should be maximized
  #return(empty.set - ret)
  return(ret[, lapply(.SD, mean)])
}
