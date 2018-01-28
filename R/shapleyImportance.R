#' @title Shapley Importance
#'
#' @description Computes the shapley importance of a feature.
#'
#' @param features [\code{character}] \cr
#' the feature(s) for which the shapley importance should be computed.
#' @param m [\code{numeric(1)} | \code{"all.unique"}] \cr
#' the number of permutations that should be used. If m > number of all unique permutatios then only all unique permutations are used. Default is \code{"all.unique"}.
#' @template arg_object
#' @param target [\code{character(1)}] \cr
#' The target feature.
#' @template arg_measures
#' @template arg_n.feat.perm
#' @template arg_local
#' @export
shapleyImportance = function(object, data, target, features, measures,
  m = "all.unique", n.feat.perm = 10, local = FALSE) {
  mid = BBmisc::vcapply(measures, function(x) x$id)
  perm = generatePermutations(features = setdiff(colnames(data), target), m = m)

  # FIXME: for each feature we compute the performance drop for the same set multiple times, at least for m = "all.unique" permutations. We should do this once.
  args = list(object = object, data = data,  measures = measures, perm = perm, n.feat.perm = n.feat.perm, local = local)
  marginal.contributions = parallelMap::parallelMap(fun = marginalContributions, f = features, more.args = args)
  mc = lapply(marginal.contributions, function(x) x$marginal.contributions)
  mc = setNames(mc, features)
  mc = data.table::rbindlist(mc, idcol = "features")

  # shapley value is the mean of all marginal contributions
  shapley.value = mc[, lapply(.SD, mean), .SDcols = mid, by = "features"]
  # variance is uncertainty, see https://github.com/slundberg/ShapleyValues.jl#least-squares-regression
  shapley.uncertainty = mc[, lapply(.SD, var), .SDcols = mid, by = "features"]

  makeS3Obj("ShapleyImportance",
    permutations = perm,
    measures = measures,
    shapley.value = shapley.value,
    shapley.uncertainty = shapley.uncertainty,
    marginal.contributions = marginal.contributions)
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
marginalContributions = function(object, data, f, measures, perm,
  n.feat.perm = 10, local = FALSE) {
  mid = BBmisc::vcapply(measures, function(x) x$id)

  set.without.f = lapply(perm, function(new.feature.order) {
    # index of feature f in permuted feature vector
    f.ind = which(new.feature.order == f)
    # features before f (excluding feature f) in alphabetical order
    if (f.ind == 1) {
      return(character(0))
    } else {
      ret = new.feature.order[1:(f.ind - 1)]
      return(ret[order(ret)])
    }
  })
  set.with.f = lapply(set.without.f, function(x) c(f, x))

  # compute performance drops only for unique sets
  unique.ind = !duplicated(set.without.f)
  imp.without.f = performanceDrop(object = object, data = data,
    features = set.without.f[unique.ind],
    measures = measures, n.feat.perm = n.feat.perm, local = local)
  imp.with.f = performanceDrop(object = object, data = data,
    features = set.with.f[unique.ind],
    measures = measures, n.feat.perm = n.feat.perm, local = local)

  # aggregate performance drops for each feature
  imp.without.f = imp.without.f[, lapply(.SD, mean), .SDcols = mid, by = "features"]
  imp.with.f = imp.with.f[, lapply(.SD, mean), .SDcols = mid, by = "features"]

  # expand performance drops if set occurs more than once
  imp.without.f = imp.without.f[match(as.character(set.without.f), imp.without.f$features),]
  imp.with.f = imp.with.f[match(as.character(set.with.f), imp.with.f$features),]

  # differences are the marginal contributions
  marginal.contributions = imp.with.f[, mid, with = FALSE] - imp.without.f[, mid, with = FALSE]
  marginal.contributions$permutations = as.character(perm)

  #return(marginal.contributions)
  makeS3Obj("MarginalContribution",
    feature = f,
    importance = unique(rbind(imp.with.f, imp.without.f)),
    marginal.contributions = marginal.contributions)
}

# generate m permutations for alle elements in features
generatePermutations = function(features, m = "all.unique") {
  assertCharacter(features)
  assert(checkSubset(m, "all.unique"), checkIntegerish(m, lower = 1))
  n.feat = length(features)

  if (m == "all.unique" | (m >= factorial(n.feat))) {
    messagef("All %s unique permuatations for the %s features were generated!", factorial(n.feat), n.feat)
    p = e1071::permutations(n.feat)
    p = lapply(seq_row(p), function(i) features[p[i,]])
  } else {
    p = lapply(1:m, function(x) sample(features))
  }
  return(p)
}

generateMarginalContribution = function(f, perm) {
  lapply(perm, function(new.feature.order) {
    # index of feature f in permuted feature vector
    f.ind = which(new.feature.order == f)
    # features before f (excluding feature f) in alphabetical order
    if (f.ind == 1) {
      without.f = character(0)
    } else {
      without.f = new.feature.order[1:(f.ind - 1)]
      #without.f = without.f[order(without.f)]
    }
    with.f = c(without.f, f)
    return(list(with.f = with.f, without.f = without.f))
  })
}

calculateValueFunction = function(features, object, data, measures,
  n.feat.perm = 10, local = FALSE) {
  mid = BBmisc::vcapply(measures, function(x) x$id)
  ret = performanceDrop(object = object, data = data, features = features,
    measures = measures, n.feat.perm = n.feat.perm, local = local)
  return(ret[, lapply(.SD, mean), .SDcols = mid, by = "features"])
}

calculateValueFunction2 = function(features, object, data, measures,
  n.feat.perm = 10, local = FALSE) {
  mid = BBmisc::vcapply(measures, function(x) x$id)
  shuffle.features = setdiff(colnames(data), features)
  ret = measurePerformance(mod = object, data = data, feature = shuffle.features,
    measures = measures, shuffle = TRUE, local = local)
  #FIXME: should be colnames(data) except target
  empty.set = measurePerformance(mod = object, data = data, feature = colnames(data),
    measures = measures, shuffle = TRUE, local = local)
  return(empty.set - ret)
}

getMarginalContributionValues = function(mc, vf) {
  mc.vals = lapply(mc, function(m) {
    f = vf$features
    ret = vf[f %in% list(m$with.f), -"features"] - vf[f %in% list(m$without.f), -"features"]
    dt.feat = data.table(features.with.f = list(m$with.f), features.without.f = list(m$without.f))
    cbind(dt.feat, ret)
  })
  rbindlist(mc.vals)
}

getShapleyImportance = function(mc.vals, measures) {
  mid = BBmisc::vcapply(measures, function(x) x$id)
  mc.vals[, lapply(.SD, mean), .SDcols = mid]
}

# Slower computation of marginal contribution as 'set' contains duplicates
# marginalContributions = function(object, data, f, measures, perm,
#   n.feat.perm = 10, local = FALSE) {
#   mid = BBmisc::vcapply(measures, function(x) x$id)
#
#   set = lapply(perm, function(new.feature.order) {
#     # index of feature f in permuted feature vector
#     f.ind = which(new.feature.order == f)
#     # features before f (including feature f)
#     with.f = new.feature.order[1:f.ind]
#     # features before f (excluding feature f)
#     without.f = setdiff(with.f, f)
#     list(with.f = with.f, without.f = without.f)
#   })
#   # FIXME: To speedup, compute performanceDrop for set = unique(lapply(unname(Reduce(c, set)), sort))
#   shap = lapply(set, function(x) {
#     with.f = x$with.f
#     without.f = x$without.f
#     imp.with.f = performanceDrop(object, data = data, features = list(with.f),
#       measures = measures, n.feat.perm = n.feat.perm, local = local)
#     imp.with.f = imp.with.f[, lapply(.SD, mean), .SDcols = mid] # by = "features"
#
#     if (length(without.f) != 0) {
#       imp.without.f = performanceDrop(object, data = data, features = list(without.f),
#         measures = measures, n.feat.perm = n.feat.perm, local = local)
#       imp.without.f = imp.without.f[, lapply(.SD, mean), .SDcols = mid]
#       ret = imp.with.f - imp.without.f
#     } else {
#       ret = imp.with.f
#     }
#     return(ret)
#   })
#   marginal.contributions =  data.table::rbindlist(setNames(shap, perm), idcol = "permutations")
#   return(marginal.contributions)
# }
#
# drawSubsetOfPowerset = function(x, m, remove.duplicates = FALSE) {
#   assertIntegerish(x)
#   n = length(x)
#   if (remove.duplicates)
#     assertIntegerish(m, lower = 1, upper = 2^n)
#
#   ret = list()
#   while (length(ret) < m) {
#     # draw a "random length"
#     len = sample(0:n, size = m, replace = TRUE, prob = choose(n, 0:n)/2^n)
#     # draw random subset of x using the "random length" and sort it to better identify duplicates
#     random.subset = lapply(len, function(l) {
#       r = sample(x, size = l)
#       r[order(r)]
#     })
#     # remove duplicates
#     if (remove.duplicates) {
#       ret = unique(c(ret, random.subset))
#     } else {
#       ret = c(ret, random.subset)
#     }
#   }
#   return(ret)
# }
#
# chooseFeatures = function(f, features, m, remove.duplicates = FALSE) {
#   ind = f == features
#   with.f = seq_along(features)
#   without.f = setdiff(with.f, which(ind))
#
#   set.without.f = drawSubsetOfPowerset(without.f, m, remove.duplicates = remove.duplicates)
#   set.without.f = lapply(set.without.f, function(ind) features[ind])
#
#   set.with.f = lapply(set.without.f, function(x) c(f, x))
#   list("with.f" = set.with.f, "without.f" = set.without.f)
# }
#
# # compute shapley value for feature 'f' using 'm' subsets
# shapley = function(f, m, object, data, measures, n.feat.perm = 10, local = FALSE, features, remove.duplicates = FALSE) {
#   set = chooseFeatures(f, features, m, remove.duplicates = remove.duplicates)
#
#   imp.with.f = performanceDrop(object, data = data, features = set$with.f, measures = measures, n.feat.perm = n.feat.perm, local = local)
#   imp.without.f = performanceDrop(object, data = data, features = set$without.f, measures = measures, n.feat.perm = n.feat.perm, local = local)
#   # FIXME: needs to be improved here
#   gain.with.f = aggregate(acc ~ features, data = imp.with.f, mean)
#   gain.without.f = aggregate(acc ~ features, data = imp.without.f, mean)
#
#   sum(gain.with.f$acc - gain.without.f$acc)/m
# }
