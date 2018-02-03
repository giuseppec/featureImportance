#' @title Shapley Importance
#'
#' @description Computes the shapley importance of a feature.
#'
#' @template arg_object
#' @template arg_data
#' @param features [\code{character}] \cr
#' the feature(s) for which the shapley importance should be computed.
#' @param target [\code{character(1)}] \cr
#' The target feature.
#' @template arg_n.feat.perm
#' @param n.shapley.perm [\code{numeric(1)} | \code{"all.unique"}] \cr
#' The number of permutations that should be used for the shapley value.
#' If n.shapley.perm > number of all unique permutatios then only all unique permutations are used.
#' Default is \code{"all.unique"}.
#' @template arg_measures
#' @param value.function [\code{function}] \cr
#' Function that defines the value function which is used to compute the shapley value.
#' @export
shapleyImportance = function(object, data, features, target, n.feat.perm = 50,
  n.shapley.perm = "all.unique", measures, value.function = calculateValueFunctionImportance) {
  assertSubset(target, colnames(data))
  measures = assertMeasure(measures)
  all.feats = setdiff(colnames(data), target)
  perm = generatePermutations(all.feats, n.shapley.perm = n.shapley.perm)

  # generate all marginal contribution sets for features where we want to compute the shapley importance
  mc.list = lapply(features, function(x) generateMarginalContribution(x, perm))
  mc = unlist(mc.list, recursive = FALSE)

  # get all unique sets
  values = unique(unname(unlist(mc, recursive = FALSE)))

  # compute value function for all unique value functions
  vf = lapply(values, function(f) {
    value.function(object = object, data = data, measures = measures,
      target = target, n.feat.perm = n.feat.perm, features = f)
  })
  vf = rbindlist(vf)
  vf$features = stri_paste_list(values, ",")

  # compute the marginal contribution values (difference of value functions)
  mc.vf = lapply(seq_along(features), function(i) {
    getMarginalContributionValues(mc.list[[i]], vf)
  })

  # get shapley importance (basically the mean of the mc.vf values)
  shapley.value = lapply(mc.vf, function(mc) {
    getShapleyImportance(mc, measures = measures)
  })

  # get shapley value uncertainty
  shapley.uncertainty = lapply(mc.vf, function(mc) {
    getShapleyUncertainty(mc, measures = measures)
  })

  makeS3Obj("ShapleyImportance",
    permutations = perm,
    measures = measures,
    shapley.value = rbindlist(setNames(shapley.value, features), idcol = "feature"),
    shapley.uncertainty = rbindlist(setNames(shapley.uncertainty, features), idcol = "feature"),
    marginal.contributions = rbindlist(setNames(mc.vf, features), idcol = "feature"))
}

print.ShapleyImportance = function(x, ...) {
  measures = collapse(vcapply(x$measures, function(m) m$id))
  #BBmisc::listToShortString(lapply(x$measures, function(m) m$id))

  catf("Object of class 'ShapleyImportance'")
  catf("Measures used: %s", measures)
  catf("Number of permutations: %s", length(x$permutations))
  catf("Shapley value(s):")
  print(x$shapley.value, ...)
}

### helper functions
# @param f [\code{character(1)}] \cr
# single feature for wich marginal contributions are computed using permutations in 'perm'
# @param perm list of permutations that are used to compute marginal contributions for

# generate n.shapley.perm permutations for alle elements in features
generatePermutations = function(features, n.shapley.perm = "all.unique") {
  assertCharacter(features)
  assert(checkSubset(n.shapley.perm, "all.unique"), checkIntegerish(n.shapley.perm, lower = 1))
  n.feat = length(features)

  if (n.shapley.perm == "all.unique" | (n.shapley.perm >= factorial(n.feat))) {
    messagef("All %s unique permuatations for the %s features were generated!", factorial(n.feat), n.feat)
    p = e1071::permutations(n.feat)
    p = lapply(seq_row(p), function(i) features[p[i,]])
  } else {
    p = lapply(1:n.shapley.perm, function(x) sample(features))
  }
  return(p)
}

generateMarginalContribution = function(f, perm) {
  lapply(perm, function(new.feature.order) {
    # index of feature f in permuted feature vector
    f.ind = which(new.feature.order == f)
    # features before f (excluding feature f) in alphabetical order
    if (f.ind == 1) {
      without.f = NA_character_
      with.f = f
    } else {
      without.f = new.feature.order[1:(f.ind - 1)]
      with.f = c(without.f, f)
    }
    return(list(with.f = with.f, without.f = without.f))
  })
}

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
  imp = imp$importance
  # aggregate importance
  imp.aggr = imp[, lapply(.SD, mean), .SDcols = mid, by = "features"]
  return(imp.aggr)
}

calculateValueFunctionPerformance = function(features, object, data, target, measures,
  n.feat.perm = 50, predict.fun = NULL) {
  assertCharacter(features)
  assertSubset(target, colnames(data))
  measures = assertMeasure(measures)

  mid = names(measures)
  all.feats = setdiff(colnames(data), target)
  # shuffle all features except the ones for which we want to compute the value function
  shuffle.features = setdiff(all.feats, features)
  # compute the value function
  ret = measurePerformance(object, data = permuteFeature(data, features = shuffle.features),
    target = target, measures = measures, predict.fun = predict.fun)
  minimize = BBmisc::vlapply(measures, function(x) x$minimize)
  if (nrow(ret) != 1)
    stopf("'ret' should be only one row.")
  ret = ifelse(minimize, -1, 1)*ret
  # we can remove empty.set which is always substracted as we use differences of value functions
  #empty.set = measurePerformance(object, data = permuteFeature(data, features = all.feats),
  #  target = target, measures = measures, predict.fun = predict.fun)
  # FIXME: ret - empty.set when measure should be maximized
  #return(empty.set - ret)
  return(ret)
}

getMarginalContributionValues = function(mc, vf) {
  mc.vals = lapply(mc, function(m) {
    # make list out of character of features of the form c("x.1,x.2", "x.2,x.3")
    f = stri_split_fixed(vf$features, ",")
    # value function with feature f
    v.with.f = vf[f %in% list(m$with.f), -"features"]
    # value function without feature f
    v.without.f = vf[f %in% list(m$without.f), -"features"]
    # marginal contribution value is the difference:
    ret = v.with.f - v.without.f
    dt.feat = data.table(features.with.f = list(m$with.f), features.without.f = list(m$without.f))
    cbind(dt.feat, ret)
  })
  rbindlist(mc.vals)
}

getShapleyImportance = function(mc.vals, measures) {
  measures = assertMeasure(measures)
  mid = names(measures)
  mc.vals[, lapply(.SD, mean), .SDcols = mid]
}

getShapleyUncertainty = function(mc.vals, measures) {
  measures = assertMeasure(measures)
  mid = names(measures)
  mc.vals[, lapply(.SD, var), .SDcols = mid]
}
