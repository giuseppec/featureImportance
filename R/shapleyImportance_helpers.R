### helper functions
# @param f [\code{character(1)}] \cr
# single feature for wich marginal contributions are computed using permutations in 'perm'
# @param perm list of permutations that are used to compute marginal contributions for

# generate n.shapley.perm permutations for alle elements in features
generatePermutations = function(features, n.shapley.perm = "all.unique", bound.size = NULL) {
  assertCharacter(features)
  assert(checkSubset(n.shapley.perm, "all.unique"), checkIntegerish(n.shapley.perm, lower = 1))
  n.feat = length(features)
  if (is.null(bound.size))
    bound.size = n.feat
  #if (is.null(bound.size))
  #  bound.size = ceiling(sqrt(length(features)))

  if (n.shapley.perm == "all.unique" | (n.shapley.perm >= factorial(n.feat))) {
    messagef("All %s unique permutations for the %s features were generated!", factorial(n.feat), n.feat)
    p = e1071::permutations(n.feat)
    p = lapply(BBmisc::seq_row(p), function(i) features[p[i,]])
  } else {
    p = lapply(1:n.shapley.perm, function(x) sample(features, size = bound.size))
  }
  return(p)
}

generateMarginalContribution = function(f, perm) {
  missing = vlapply(perm, function(x) {
    f %nin% x
  })
  perm = perm[!missing]

  lapply(perm, function(new.feature.order) {
    # index of feature f in permuted feature vector
    f.ind = which(new.feature.order == f)
    # features before f (excluding feature f) in alphabetical order
    if (f.ind == 1) {
      without.f = NA_character_
      with.f = f
    } else {
      # sorting speeds up everything as we can use the same value function, e.g. for X1,X2 and X2,X1
      without.f = sort(new.feature.order[1:(f.ind - 1)])
      with.f = sort(c(without.f, f)) #with.f = c(without.f, f)
    }
    return(list(with.f = with.f, without.f = without.f))
  })
}

# getMarginalContributionValues = function(mc, vf) {
#   f = vf$features
#   vf = vf[, -"features"]
#   mc.vals = lapply(mc, function(m) {
#     # make list out of character of features of the form c("x.1,x.2", "x.2,x.3")
#     with.f = stri_paste(m$with.f, collapse = ",")
#     without.f = stri_paste(m$without.f, collapse = ",")
#     # value function with feature f
#     v.with.f = vf[charmatch(with.f, f), ] #vf[f %in% with.f,]
#     # value function without feature f
#     v.without.f = vf[charmatch(without.f, f), ] #vf[f %in% without.f,]
#     # marginal contribution value is the difference:
#     ret = v.with.f - v.without.f
#     dt.feat = data.table(features.with.f = with.f, features.without.f = without.f, ret)
#     #cbind(dt.feat, ret)
#   })
#   rbindlist(mc.vals)
# }

getMarginalContributionValues = function(mc, vf) {
  f = vf$features
  vf = vf[, -"features"]
  mc.vals = lapply(mc, function(m) {
    # make string to match with f
    with.f = stri_paste(m$with.f, collapse = ",")
    without.f = stri_paste(m$without.f, collapse = ",")
    # value function with feature f
    v.with.f = vf[charmatch(with.f, f), ] #vf[f %in% with.f,]
    # value function without feature f
    v.without.f = vf[charmatch(without.f, f), ] #vf[f %in% without.f,]
    list(
      with.f = with.f,
      without.f = without.f,
      v.with.f = v.with.f,
      v.without.f = v.without.f
    )
  })

  # extract string
  with.f = vcapply(mc.vals, function(x) x$with.f)
  without.f = vcapply(mc.vals, function(x) x$without.f)

  # extract value functions
  v.with.f = rbindlist(lapply(mc.vals, function(x) x$v.with.f))
  v.without.f = rbindlist(lapply(mc.vals, function(x) x$v.without.f))

  # compute marginal contribution value which is the difference of value functions:
  ret = v.with.f - v.without.f
  return(data.table(features.with.f = with.f, features.without.f = without.f, ret))
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
