#' @import BBmisc
#' @import checkmate
#' @import data.table
#' @import ggplot2
#' @import parallelMap
#' @import stringi
#' @importFrom stats predict setNames var
#' @importFrom utils type.convert
NULL

.onLoad = function(libname, pkgname) {
  backports::import(pkgname)
  # parallelMap::parallelRegisterLevels(package = "featureImportance",
  #   levels = c("n.target.perm", "n.feat.perm"))
}
