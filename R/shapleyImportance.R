#' @title Shapley Importance
#'
#' @description Computes the shapley importance of a feature.
#' @references Cohen, S., Dror, G., & Ruppin, E. (2007).
#' Feature selection via coalitional game theory.
#' Neural Computation, 19(7), 1939-1961.
#'
#' @inheritParams featureImportance
#' @param features [\code{character}] \cr
#' The feature(s) for which the shapley importance should be computed.
#' @param bound.size [\code{numeric(1)}] \cr
#' Bound on the permutation size to compute the Shapley value (see Cohen (2007)).
#' @param n.shapley.perm [\code{numeric(1)} | \code{"all.unique"}] \cr
#' The number of permutations that should be used for the shapley value.
#' Use \code{"all.unique"} to use all unique permutations.
#' If n.shapley.perm > number of all unique permutatios then only all unique permutations are used.
#' Default is 120.
#' @param value.function [\code{function}] \cr
#' Function that defines the value function which is used to compute the shapley value.
#' @export
shapleyImportance = function(object, data, features, target, bound.size = NULL,
  n.feat.perm = 50, n.shapley.perm = 120, measures,
  value.function = calculateValueFunctionImportance) {
  assertSubset(features, colnames(data))
  assertSubset(target, colnames(data))
  measures = assertMeasure(measures)
  all.feats = setdiff(colnames(data), target)
  perm = generatePermutations(all.feats, n.shapley.perm = n.shapley.perm,
    bound.size = bound.size)

  # generate all marginal contribution sets for features where we want to compute the shapley importance
  mc.list = lapply(features, function(x) generateMarginalContribution(x, perm))
  mc = unlist(mc.list, recursive = FALSE)

  # get all unique sets
  values = unique(unname(unlist(mc, recursive = FALSE)))

  # compute value function for all unique value functions
  # FIXME: allow parallelization
  vf = pbapply::pblapply(values, function(f) {
    opb = pboptions(type = "none")
    on.exit(pboptions(opb))
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
    value.function = vf,
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
