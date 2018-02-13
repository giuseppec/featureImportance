#' @import BBmisc
#' @import checkmate
#' @import data.table
#' @import mlr
#' @import parallelMap
#' @import stringi
#' @import pbapply
#' @importFrom stats predict setNames var
NULL

.onLoad = function(libname, pkgname) {
  backports::import(pkgname)
  # parallelMap::parallelRegisterLevels(package = "featureImportance",
  #   levels = c("n.target.perm", "n.feat.perm"))
}
