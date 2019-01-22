#' @title Plot function for feature importance
#'
#' @description Allows to visualize the PI or ICI curves.
#' @references Casalicchio, G., Molnar, M., & Bischl, B. (2018).
#' Visualizing the Feature Importance for Black Box Models.
#' arXiv preprint arXiv:1804.06620 (2018).
#'
#' @param importance [\code{featureImportance | data.frame}] \cr
#' Object of class \code{featureImportance} or a \code{data.frame} containing the same information as the result of the \code{featureImportance}'s \code{$importance} slot.
#' @param feature [\code{character(1)}] \cr
#' The feature for which the PI or ICI curves should be visualized.
#' @param mid [\code{character(1)}] \cr
#' The measure name used for computing the feature importance.
#' @param individual [\code{logical(1)}] \cr
#' If \code{TRUE}, the ICI curves are additionally plotted. Otherwise only the PI curve is plotted.
#' @param hline [\code{logical(1)}] \cr
#' Whether a horizontal line should be plotted whose y-position refers to the feature importance of the considered feature.
#' @param grid.points [\code{logical(1)}] \cr
#' Whether points should be plotted whose x-position refer to the feature values used to compute the curves.
#' @export
plotImportance = function(importance, feature, mid, individual = FALSE, hline = TRUE, grid.points = TRUE) {
  if (inherits(importance, "featureImportance")) {
    method = importance$method
    local = importance$local
    if (!local & importance$method == "permute")
      stop("Local feature importance was not computed.")
    if (!local & individual)
      stop("'individual = TRUE' not possible if no local feature importance was computed.")
    importance = importance$importance
  }
  assertDataFrame(importance)
  assertString(feature)
  assertString(mid)
  assertFlag(individual)
  assertFlag(hline)
  assertFlag(grid.points)

  d = copy(subset(importance, importance$features %in% feature))
  title = ifelse(individual, "ICI plot", "PI plot")
  by = c("replace.id", "features", "feature.value")
  pi = d[, lapply(.SD, mean, na.rm = TRUE), .SDcols = mid, by = by]

  pp = ggplot(data = pi, aes_string(x = "feature.value", y = mid))
  if (individual)
    pp = pp + geom_line(data = d, aes_string(x = "feature.value", y = mid, group = "row.id"), color = "gray")
  if (hline)
    pp = pp + geom_hline(yintercept = mean(pi[[mid]]))
  if (grid.points)
    pp = pp + geom_point()

  if (method == "permute")
    pp = pp + geom_smooth(col = "black") else
      pp = pp + geom_line()

  pp + labs(title = title, x = feature, y = bquote(Delta~L ~ "based on" ~ .(toupper(mid))))
}
