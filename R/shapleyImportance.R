shapleyImportance = function(f, m = "all.unique", object, data, features, measures, n.feat.perm = 10, local = FALSE) {
  mid = BBmisc::vcapply(measures, function(x) x$id)
  perm = generatePermutations(features, m)

  # set = lapply(perm, function(new.feature.order) {
  #   # index of feature f in permuted feature vector
  #   f.ind = which(new.feature.order == f)
  #   # features before f (including feature f)
  #   with.f = new.feature.order[1:f.ind]
  #   # features before f (excluding feature f)
  #   without.f = setdiff(with.f, f)
  #   list(with.f = with.f, without.f = without.f)
  # })
  # # FIXME: speedup -> we only need to compute performanceDrop for the set: unique(lapply(unname(Reduce(c, set)), sort))
  # shap = lapply(set, function(x) {
  #   with.f = x$with.f
  #   without.f = x$without.f
  #   imp.with.f = performanceDrop(object, data = data, features = list(with.f),
  #     measures = measures, n.feat.perm = n.feat.perm, local = local)
  #   imp.with.f = imp.with.f[, lapply(.SD, mean), .SDcols = mid] # by = "features"
  #
  #   if (length(without.f) != 0) {
  #     imp.without.f = performanceDrop(object, data = data, features = list(without.f),
  #       measures = measures, n.feat.perm = n.feat.perm, local = local)
  #     imp.without.f = imp.without.f[, lapply(.SD, mean), .SDcols = mid]
  #     ret = imp.with.f - imp.without.f
  #   } else {
  #     ret = imp.with.f
  #   }
  #   return(ret)
  # })
  # marginal.contributions = rbindlist(setNames(shap, perm), idcol = "permutations")

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
  imp.without.f = performanceDrop(object, data = data, features = set.without.f[unique.ind],
    measures = measures, n.feat.perm = n.feat.perm, local = local)
  imp.with.f = performanceDrop(object, data = data, features = set.with.f[unique.ind],
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

  # shapley value is the mean of all marginal contributions
  shapley.value = marginal.contributions[, lapply(.SD, mean), .SDcols = mid]

  makeS3Obj("ShapleyImportance",
    n.permutations = length(perm),
    feature = f,
    #n.unique.permutations = sum(unique.ind),
    shapley.value = shapley.value,
    marginal.contributions = marginal.contributions)
}

generatePermutations = function(features, m = "all.unique") {
  assert(checkSubset(m, "all.unique"), checkIntegerish(m, lower = 1))
  n.feat = length(features)

  if (m == "all.unique" | (m >= 2^n.feat)) {
    messagef("As there are %s features, all %s unique permuatations will be generated!", n.feat, 2^n.feat)
    p = e1071::permutations(n.feat)
    p = lapply(seq_row(p), function(i) features[p[i,]])
  } else {
    p = lapply(1:m, function(x) sample(features))
  }
  return(p)
}

print.ShapleyImportance = function(x) {
  catf("ShapleyImportance:")
  catf(" Number of permutations: %s", x$n.permutations)
  catf(" Shapley value for '%s': \n  %s", x$feature, BBmisc::listToShortString(as.list(shap$shapley.value)))
  #print(x$shapley.value)
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
