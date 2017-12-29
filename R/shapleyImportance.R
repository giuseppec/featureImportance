shapleyImportance = function(f, m = "all.unique", object, data, features, measures, n.feat.perm = 10, local = FALSE) {
  perm = generatePermutations(features, m)
  set = lapply(perm, function(new.feature.order) {
    # index of feature f in permuted feature vector
    f.ind = which(new.feature.order == f)
    # features before f (including feature f)
    with.f = new.feature.order[1:f.ind]
    # features before f (excluding feature f)
    without.f = setdiff(with.f, f)
    list(with.f = with.f, without.f = without.f)
  })

  # FIXME: speedup -> we only need to compute performanceDrop for the set: unique(unname(Reduce(c, set)))
  shap = lapply(set, function(x) {
    with.f = x$with.f
    without.f = x$without.f
    imp.with.f = performanceDrop(object, data = data, features = list(with.f),
      measures = measures, n.feat.perm = n.feat.perm, local = local)
    imp.with.f = imp.with.f[, as.list(unlist(lapply(.SD, mean))), by = "features"]
    imp.with.f = imp.with.f[ ,`:=`(cv.iter = NULL, n.feat.perm = NULL, features = NULL)]

    if (length(without.f) != 0) {
      imp.without.f = performanceDrop(object, data = data, features = list(without.f),
        measures = measures, n.feat.perm = n.feat.perm, local = local)
      imp.without.f = imp.without.f[, as.list(unlist(lapply(.SD, mean))), by = "features"]
      imp.without.f = imp.without.f[ ,`:=`(cv.iter = NULL, n.feat.perm = NULL, features = NULL)]
      ret = imp.with.f - imp.without.f
    } else {
      ret = imp.with.f
    }
    #c(list(BBmisc::collapse(new.feature.order)), as.list(unlist(ret)))
    #ret$features = BBmisc::collapse(new.feature.order)
    return(ret)
  })

  marginal.contributions = tmp = rbindlist(setNames(shap, perm), idcol = "permutations")
  tmp = tmp[, permutations := NULL]
  shapley.value = tmp[, as.list(unlist(lapply(.SD, mean)))]

  makeS3Obj("ShapleyImportance",
    shapley.value = shapley.value,
    marginal.contributions = marginal.contributions)
}

generatePermutations = function(features, m = "all.unique") {
  assert(checkSubset(m, "all.unique"), checkIntegerish(m, lower = 1))
  if (m == "all.unique") {
    p = e1071::permutations(length(features))
    p = lapply(seq_row(p), function(i) features[p[i,]])
  } else {
    p = lapply(1:m, function(x) sample(features))
  }
  return(p)
}

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
