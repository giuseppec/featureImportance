#' @param importance.fun [\code{function}] \cr
#' Function with signature \code{function(permuted, unpermuted, minimize)} which defines how the permuted and unpermuted predictions are aggregated to a feature importance measure.
#' The function takes the result of \code{\link{measurePerformance}} as input for \code{permuted} and \code{unpermuted}.
#' The argument \code{minimize} can be used in \code{importance.fun} to change the aggregation behaivour depending on whether the measure is to be minimized or not, e.g. for the drop in performance: \cr
#' \code{ifelse(minimize, -1, 1) * (unpermuted - permuted)} \cr
#' The default \code{NULL} internally uses \code{permuted - unpermuted} (or \code{unpermuted - permuted}, depending on whether the measure is to be minimized or not) which refers to the drop in performance.
