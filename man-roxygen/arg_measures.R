#' @param measures [\code{\link[mlr]{Measure}} | list of \code{\link[mlr]{Measure}} | \code{function} | list of \code{function}] \cr
#' Performance measure(s) used to measure the model performance.
#' Can also be a list of function with signature \code{function(y, pred, minimize)},
#' where \code{y} and \code{pred} are vectors containing the true and the predicted values of the target,
#' and \code{minimize} is a logical that defines if smaller values of the measure refer to better performance.
