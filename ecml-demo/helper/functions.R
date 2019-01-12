cartesian = function(data, features, target, keep.na = TRUE) {
  if (length(features) == 0)
    return(data)
  n = nrow(data)
  row.indices = rep(1:n, times = n)
  replace.indices = rep(1:n, each = n)
  keep.indices = row.indices != replace.indices
  data = data[row.indices, ]
  data[features] = data[replace.indices, features]
  if (keep.na) data[!keep.indices, target] = NA
  cbind("obs.id" = row.indices, "replace.id" = replace.indices, data)
}

vGE = function(features, object, data, target = NULL,
  n.feat.perm = 50, measures, predict.fun = NULL, local = FALSE) {
  #assertCharacter(features)

  all.feats = setdiff(colnames(data), target)
  # shuffle all features except the ones for which we want to compute the value function
  shuffle.features = setdiff(all.feats, features)
  #shuffle.features = if (is.na(shuffle.features)) character(0) else shuffle.features
  # compute the value function
  data.perm = cartesian(data, features = shuffle.features, target, keep.na = FALSE)
  # measure performance on test data with replaced values
  geS = measurePerformance(object, data = na.omit(data.perm),
    target = target, measures = measures, predict.fun = predict.fun, local = local)

  d0 = cartesian(data, features = all.feats, target, keep.na = FALSE)
  ge0 = measurePerformance(object, data = d0,
    target = target, measures = measures, predict.fun = predict.fun, local = local)

  return(cbind(features = stri_paste(features, collapse = ","), geS - ge0))
}

pfi = function(mod, data, target, measures, features) {
  require(featureImportance)
  if (!is.list(features))
    features = as.list(features)

  # measure performance on test data with replaced values
  pfi.diff = featureImportance(mod, data = data, features = features,
    target = target, measures = measures, local = FALSE, replace.ids = 1:nrow(data))

  # measure PFI by taking ratio
  pfi.ratio = featureImportance(mod, data = data, features = features,
    target = target, measures = measures, local = FALSE, replace.ids = 1:nrow(data),
    importance.fun = function(permuted, unpermuted) {permuted / unpermuted})

  list(pfi.diff = pfi.diff, pfi.ratio = pfi.ratio)
}

ge = function(mod, data, target, measures, features) {
  require(featureImportance)
  geP = measurePerformance(mod, data = data, target = target, measures = measures)

  all.feats = setdiff(colnames(data), target)
  d0 = cartesian(data, features = all.feats, target, keep.na = FALSE)
  ge0 = measurePerformance(mod, data = d0, target = target, measures = measures)

  list(geP = geP, ge0 = ge0)
}

# function to plot PI and ICI curves
plotPartialImportance = function(pfi, feat, mid, individual = FALSE, rug = TRUE, hline = TRUE,
  grid.points = TRUE, subset.observation.index = NULL, subset.replaced.index = NULL) {
  d = copy(subset(pfi, features == feat))
  by = c("replace.id", "features", "feature.value")
  if (!is.null(subset.observation.index))
    d = subset(d, row.id %in% subset.observation.index)
  if (!is.null(subset.replaced.index))
    d = subset(d, replace.id %in% subset.replaced.index)

  pi = d[, lapply(.SD, mean, na.rm = TRUE),
    .SDcols = c(mid), by = by]

  pp = ggplot(data = as.data.frame(pi), aes_string(x = "feature.value", y = mid))
  if (grid.points)
    pp = pp + geom_point()
  if (individual) {
    pp = pp + geom_point(shape = NA) + geom_line(data = as.data.frame(na.omit(d)),
      aes_string(x = "feature.value", y = mid, group = "row.id"), color = "gray")
  }
  pp = pp + geom_line(data = as.data.frame(pi), aes_string(x = "feature.value", y = mid))
  if (rug)
    pp = pp + geom_rug()
  if (hline)
    pp = pp + geom_hline(yintercept = mean(pi[[mid]]))
  return(pp)
}

# function for recomputing the feature importance after removing observations indexed by subset.ind
getImpTable = function(pfi, obs.id = NULL, mid = "mse", sort = TRUE) {
  if (!is.null(obs.id))
    pfi = subset(pfi, row.id %in% obs.id & replace.id %in% obs.id)
  imp = pfi[, lapply(.SD, mean, na.rm = TRUE), .SDcols = mid, by = "features"]
  if (sort)
    imp = imp[order(get(mid), decreasing = TRUE)]
  ret = round(transpose(imp[, -"features"]), 1)
  setnames(ret, names(ret), imp$features)
}

pasteMeanSd = function(x) {
  x.mean = mean(x)
  if (x.mean != 0)
    paste0(round(x.mean, 2), " (", round(sd(x), 2), ")") else
      x.mean
}

conditionalPFI = function(pfi, var, group0, group1, group.var,
  mid = "mse", aggregate = TRUE) {
  by = c("replace.id", "features", "feature.value", group.var)
  ici = subset(pfi, features == var)
  ici[[group.var]] = as.factor(as.numeric(ici$row.id %in% group1))
  pi = rbind(
    subset(ici, row.id %in% group0 & replace.id %in% group0),
    subset(ici, row.id %in% group1 & replace.id %in% group1)
  )
  if (aggregate)
    pi = pi[, lapply(.SD, mean, na.rm = TRUE), .SDcols = mid, by = by]
  return(pi)
}
