#' An S4 Class to hold parameters needed to define TimefitteR data models
#'
#' TimefitteR has the ability to generate semi-deterministic time series data sets. This class holds the parameters used to create these data sets for each level of a defined data structure.
#'
#'
#' @slot intercept_offset Numeric to control the intercept of the series for this grouping
#' @slot intercept_noise numeric multiplier to apply to the standard noise level for the intercept for this grouping.
#' @slot slope_offset numeric to control the slope of the series for this grouping.
#' @slot slope_noise numeric multiplier to apply to the standard noise level for the slope for this grouping.
#' @slot curvature_offset numeric to control the curvature of the series for this grouping.
#' @slot curvature_noise numeric multiplier to apply to the standard noise level for the curvature for this grouping.
#'
#' @name TimefitteR_Param_List
#' @rdname TimefitteR_Param_List
#' @exportClass TimefitteR_Param_List
#'
#' @examples
#' x <- new("TimefitterR_Param_List")

setClass(
  "TimefitteR_Param_List",
  slots = list(
    intercept_offset = "numeric",
    intercept_noise = "numeric",
    slope_offset = "numeric",
    slope_noise = "numeric",
    curvature_offset = "numeric",
    curvature_noise = "numeric"
  ),
  prototype = list(
    intercept_offset = 1,
    intercept_noise = 1,
    slope_offset = 0,
    slope_noise = 1,
    curvature_offset = 0,
    curvature_noise = 1
  )
)
