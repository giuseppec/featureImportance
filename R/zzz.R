#' @import BBmisc
#' @import checkmate
#' @import data.table
#' @import mlr
#' @import parallelMap
#' @import stringi
#' @import pbapply
#' @importFrom stats predict setNames var

.onAttach = function(libname, pkgname) {
  parallelMap::parallelRegisterLevels(package = "featureImportance",
    levels = c("feature.permutation", "target.permutation"))
}
