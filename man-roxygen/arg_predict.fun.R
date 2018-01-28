#' @param predict.fun [\code{function}] \cr
#' Only needed if \code{object} is not of class \code{WrappedModel} or \code{ResampleResult}.
#' The signature must be \code{function(object, newdata)} and the function should always return a vector of predictions.
#' The default \code{NULL} internally uses \code{predict(object, newdata = newdata)}.
#'
