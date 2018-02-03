context("shapleyImportance helper checks (glove game)")
test_that("shapleyImportance helper checks (glove game)", {
  features = c("1", "2", "3")
  perm = generatePermutations(features)
  n = factorial(length(features))
  expect_list(perm, len = n)
  expect_identical(perm, generatePermutations(features, n.shapley.perm = 100))
  expect_true(all(generatePermutations(features, n.shapley.perm = 2) %in% perm))

  # get all unique sets for "1"
  mc = generateMarginalContribution("1", perm)
  expect_list(mc, len = n)
  for (i in seq_along(mc))
    expect_named(mc[[i]], c("with.f", "without.f"))

  calculateValueFunction = function(features){
    val = list(c("1", "3"), c("2", "3"), c("1", "2", "3"))
    if (any(BBmisc::vlapply(val, function(x) all(x %in% features))))
      return(1) else
        return(0)
  }

  mc = lapply(features, function(x) generateMarginalContribution(x, perm))
  values = unique(unname(unlist(unlist(mc, recursive = FALSE), recursive = FALSE)))
  value.function = lapply(values, function(f) {
    as.data.table(calculateValueFunction(f))
  })
  vf = rbindlist(value.function)
  vf$features = stri_paste_list(values, ",")

  # see here https://en.wikipedia.org/wiki/Shapley_value#Example
  expect_identical(mean(getMarginalContributionValues(generateMarginalContribution("1", perm), vf)$V1), 1/6)
  expect_identical(mean(getMarginalContributionValues(generateMarginalContribution("2", perm), vf)$V1), 1/6)
  expect_identical(mean(getMarginalContributionValues(generateMarginalContribution("3", perm), vf)$V1), 4/6)
})
