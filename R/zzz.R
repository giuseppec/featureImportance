#' @import BBmisc
#' @import checkmate
#' @import data.table
#' @import mlr
#' @import parallelMap
#' @import stringi
#' @importFrom stats predict setNames

.onAttach = function(libname, pkgname) {
  parallelMap::parallelRegisterLevels(package = "featureImportance",
    levels = c("feature.permutation", "target.permutation"))
}
