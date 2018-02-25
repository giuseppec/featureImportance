#' @title List all implemented performance measures
#'
#' @description Lists all implemented performance measures that can be used to measure the feature importance
#'
#' @param properties [\code{character}] \cr
#' Set of measure properties. Currently, some standard property names include:
#' \describe{
#'   \item{classif}{Is the measure applicable for classification?}
#'   \item{classif.multi}{Is the measure applicable for multi-class classification?}
#'   \item{regr}{Is the measure applicable for regression?}
#'   \item{req.pred}{Is prediction object required in calculation? Usually the case.}
#'   \item{req.truth}{Is truth column required in calculation? Usually the case.}
#'   \item{req.prob}{Are predicted probabilities required in calculation? Usually not the case, example would be AUC.}
#' }
#' @return [\code{data.frame}] \cr
#' #' Object slots:
#' \describe{
#'   \item{id (`character(1)`)}{Short id of the measure.}
#'   \item{name (`character(1)`)}{Name of the measure.}
#'   \item{minimize (`logical(1)`)}{Should the measure be minimized?}
#'   \item{best (`numeric(1)`)}{Best obtainable value for the performance measure.}
#'   \item{worst (`numeric(1)`)}{Worst obtainable value for the performance measure.}
#'   \item{note (`character(1)`)}{Description and additional notes for the performance measure.}
#'   \item{properties (`character(1)`)}{Properties of the performance measure, each property separated by a comma.}
#' }
#' @export
listPerformanceMeasures = function(properties = NULL) {
  prop = c("classif", "classif.multi", "regr", "req.pred", "req.truth", "req.prob")
  assertSubset(properties, choices = prop, empty.ok = TRUE)
  res = getPerformanceMeasures()
  p = lapply(res, function(x) x$Measure$properties)
  if (any(unique(unlist(p)) %nin% prop))
    stop("'getPerformanceMeasures()' contains measures with unsupported properties.")
  if (!is.null(properties))
    ind = vlapply(p, function(x) any(x %in% properties)) else
      ind = TRUE
  res = lapply(res[ind], function(x) {
    ret = (x$Measure[c("id", "name", "minimize", "best", "worst", "note")])
  })
  res = rbindlist(res)
  res$properties = BBmisc::vcapply(p[ind], stri_paste, collapse = ", ")
  return(res)
}

#' @title Get list of performance measures
#'
#' @description Get list of performance measures by id
#' @param id [\code{character}] \cr
#' One or more measure IDs, see \code{listPerformanceMeasures()$id} for possible values.
#' @param object.type [\code{character}] \cr
#' Possible values are \code{"Measure"} or \code{"function"}.
#' Whether the chosen performance measure should be returned as a function or as a \code{\link[mlr]{Measure}} object.
#' The default is \code{NULL} and returns a list containing both, \code{"Measure"} and \code{"function"}.
#' @return [\code{list}] \cr
#' A named list of perfromance measures.
#' Depending on \code{object.type}, each list element is either of class \code{\link[mlr]{Measure}} or \code{function} or again a list with names \code{"Measure"} and \code{"function"} containing both object.
#' @export
getPerformanceMeasures = function(id = NULL, object.type = NULL) {
  assertChoice(object.type, choices = c("Measure", "function"), null.ok = TRUE)
  res = list(
    # Regression
    "mae" = list("Measure" = mlr::mae, "function" = mlr::measureMAE),
    "mse" = list("Measure" = mlr::mse, "function" = mlr::measureMSE),
    "rmse" = list("Measure" = mlr::rmse, "function" = mlr::measureRMSE),
    "msle" = list("Measure" = mlr::msle, "function" = mlr::measureMSLE),
    "sse" = list("Measure" = mlr::sse, "function" = mlr::measureSSE),
    "rsq" = list("Measure" = mlr::rsq, "function" = mlr::measureRSQ),
    "mape" = list("Measure" = mlr::mape, "function" = mlr::measureMAPE),
    #  classif,classif.multi,req.pred,req.truth:
    "mmce" = list("Measure" = mlr::mmce, "function" = mlr::measureMMCE),
    "acc" = list("Measure" = mlr::acc, "function" = mlr::measureACC),
    "ber" = list("Measure" = mlr::ber, "function" = function(truth, response) {
      mean(diag(1 - (table(truth, response) / table(truth, truth))))
    }),
    "kappa" = list("Measure" = mlr::kappa, "function" = mlr::measureKAPPA),
    "wkappa" = list("Measure" = mlr::wkappa, "function" = mlr::measureWKAPPA),
    # classif,req.pred,req.truth
    "fpr" = list("Measure" = mlr::fpr, "function" = function(truth, response) {
      mlr::measureFPR(truth, response, positive = levels(truth)[1])
    }),
    "tpr" = list("Measure" = mlr::tpr, "function" = function(truth, response) {
      mlr::measureTPR(truth, response, positive = levels(truth)[1])
    }),
    # Classification with probabilities
    "logloss" = list("Measure" = mlr::logloss, "function" = mlr::measureLogloss),
    "auc" = list("Measure" = mlr::auc, "function" = function(probabilities, truth) {
      levels = levels(truth)
      if (length(levels) != 2)
        stop("AUC only possible for two-class.")
      probabilities = probabilities[, levels]
      mlr::measureAUC(probabilities, truth, positive = levels[1], negative = levels[2])
    })
  )
  assertSubset(id, choices = names(res), empty.ok = TRUE)
  if (!is.null(id))
    res = res[id]
  if (!is.null(object.type))
    res = unlist(lapply(res, function(x) unname(x[object.type])), recursive = FALSE)
  return(res)
}
