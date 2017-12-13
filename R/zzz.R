#' @import BBmisc
#' @import checkmate
#' @import data.table
#' @import parallelMap
#' @import stringi

.onAttach = function(libname, pkgname) {
  parallelMap::parallelRegisterLevels(package = "featureImportance",
    levels = c("feature.permutation", "target.permutation"))
}
