### helper functions
# @param f [\code{character(1)}] \cr
# single feature for wich marginal contributions are computed using permutations in 'perm'
# @param perm list of permutations that are used to compute marginal contributions for

# generate n.shapley.perm permutations for alle elements in features
generatePermutations = function(features, n.shapley.perm = NULL, bound.size = NULL) {
  assertCharacter(features)
  perm.limit = 8192L
  n.feat = length(features)
  if (is.null(n.shapley.perm))
    n.shapley.perm = min(factorial(n.feat), perm.limit)
  assertIntegerish(n.shapley.perm, lower = 1, upper = perm.limit)
  assertIntegerish(bound.size, lower = 1, upper = n.feat, null.ok = TRUE)
  if (is.null(bound.size))
    bound.size = n.feat
  #if (is.null(bound.size))
  #  bound.size = ceiling(sqrt(length(features)))

  all.perm = n.shapley.perm >= factorial(n.feat)
  no.bound = bound.size == n.feat
  if (all.perm & no.bound) {
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
    v.with.f = vf[f %in% with.f,] #vf[charmatch(with.f, f), ]
    if (!is.null(v.with.f$row.id))
      v.with.f = setkeyv(v.with.f, "row.id")
    # value function without feature f
    v.without.f = vf[f %in% without.f,] #vf[charmatch(without.f, f), ]
    if (!is.null(v.without.f$row.id))
      v.without.f = setkeyv(v.without.f, "row.id")
    list(
      with.f = with.f,
      without.f = without.f,
      v.with.f = v.with.f,
      v.without.f = v.without.f
    )
  })

  # extract string
  #with.f = vcapply(mc.vals, function(x) x$with.f)
  #without.f = vcapply(mc.vals, function(x) x$without.f)

  # extract value functions
  v.with.f = rbindlist(lapply(mc.vals,
    function(x) x$v.with.f[, features.with.f := x$with.f]))
  v.without.f = rbindlist(lapply(mc.vals,
    function(x) x$v.without.f[, features.without.f := x$without.f]))

  if (!is.null(v.with.f$row.id) & !is.null(v.without.f$row.id)) {
    rid = "row.id"
    mid = setdiff(colnames(vf), rid)
    v.with.f = v.with.f[, lapply(.SD, mean), .SDcols = mid, by = c(rid, "features.with.f")]
    #v.with.f = setkeyv(v.with.f, rid)
    v.without.f = v.without.f[, lapply(.SD, mean), .SDcols = mid, by = c(rid, "features.without.f")]
    #v.without.f = setkeyv(v.without.f, rid)
  } else {
    mid = colnames(vf)
  }

  # compute marginal contribution value which is the difference of value functions:
  ret = v.with.f[, mid, with = FALSE] - v.without.f[, mid, with = FALSE]
  ret = data.table(features.with.f = v.with.f$features.with.f,
    features.without.f = v.without.f$features.without.f, ret)

  if (!is.null(v.with.f$row.id) & !is.null(v.without.f$row.id))
    ret$row.id = v.with.f$row.id

  return(ret)
}

getShapleyImportance = function(mc.vals) {
  #measures = assertMeasure(measures)
  mid = setdiff(colnames(mc.vals), c("feature", "features.with.f", "features.without.f", "row.id"))
  if (is.null(mc.vals$row.id))
    mc.vals[, lapply(.SD, mean), .SDcols = mid] else
      mc.vals[, lapply(.SD, mean), .SDcols = mid, by = "row.id"]
}

getShapleyUncertainty = function(mc.vals) {
  #measures = assertMeasure(measures)
  mid = setdiff(colnames(mc.vals), c("feature", "features.with.f", "features.without.f", "row.id"))
  if (is.null(mc.vals$row.id))
    mc.vals[, lapply(.SD, var), .SDcols = mid] else
      mc.vals[, lapply(.SD, var), .SDcols = mid, by = "row.id"]
}
