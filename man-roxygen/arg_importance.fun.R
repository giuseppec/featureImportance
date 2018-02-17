#' @param importance.fun [\code{function}] \cr
#' Function with signature \code{function(permuted, unpermuted)} which defines how \code{permuted} and \code{unpermuted} are aggregated to a feature importance measure.
#' The function takes the result of \code{\link{measurePerformance}} as input for \code{permuted} and \code{unpermuted}.
#' The default \code{NULL} internally uses \code{unpermuted - permuted} which refers to the drop in performance if the measure is to be maximized (i.e., if higher values of the measure refer to better performance).
