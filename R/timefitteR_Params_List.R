#' Create a New timefitteR_Param_List Object
#'
#' This is used internally to ensure that parameter lists are complete.
#'
#' @param intercept_offset Numeric to control the intercept of the series for this grouping
#' @param intercept_noise numeric multiplier to apply to the standard noise level for the intercept for this grouping.
#' @param slope_offset numeric to control the slope of the series for this grouping.
#' @param slope_noise numeric multiplier to apply to the standard noise level for the slope for this grouping.
#' @param curvature_offset numeric to control the curvature of the series for this grouping.
#' @param curvature_noise numeric multiplier to apply to the standard noise level for the curvature for this grouping.
#'
#' @return timefitteR_Param_List
#'
#' @examples
#' x <- new_timefitteR_Param_List(0,1,0,1,0,1)
#'
new_timefitteR_Param_List <- function(
    intercept_offset = double(),
    intercept_noise = double(),
    slope_offset = double(),
    slope_noise = double(),
    curvature_offset = double(),
    curvature_noise = double()){

  stopifnot(is.double(intercept_offset),
            is.double(intercept_noise),
            is.double(slope_offset),
            is.double(slope_noise),
            is.double(curvature_offset),
            is.double(curvature_noise),
            intercept_noise>=0,
            slope_noise>=0,
            curvature_noise>=0)

  structure(list(
    intercept_offset = intercept_offset,
    intercept_noise = intercept_noise,
    slope_offset = slope_offset,
    slope_noise = slope_noise,
    curvature_offset = curvature_offset,
    curvature_noise = curvature_noise),
    class = "timefitteR_Param_List"
  )
}

#' Validate a timefitteR_Param_List object
#'
#' @param x a timefitteR_Param_List object
#'
#' @return a timefitteR_Param_List object or error
#'
#' @examples
#' x <- new_timefitteR_Param_list(0,1,0,1,0,1)
#' validate_timefitteR_Param_List(x)
validate_timefitteR_Param_List <- function(x){
  values <- unclass(x)

  if(length(values)!=6 || !is.list(x)){
    stop("The parameter list is incomplete.")
  }
  if (!all(!is.na(values)) && !all(is.double(values))){
    stop(
      "All parameters must be non-missing and doubles!"
    )
  }
  if(x$intercept_noise<0 || x$slope_noise<0 || x$curvature_noise<0){
    stop(
      "All noise parameters must be zero or positive numbers!"
    )
  }
  x
}

#' Create a New timefitteR_Param_List Object
#'
#' This is used to create compliant parameter lists for data models. Default values add no variation.
#'
#' @param intercept_offset Numeric to control the intercept of the series for this grouping
#' @param intercept_noise numeric multiplier to apply to the standard noise level for the intercept for this grouping.
#' @param slope_offset numeric to control the slope of the series for this grouping.
#' @param slope_noise numeric multiplier to apply to the standard noise level for the slope for this grouping.
#' @param curvature_offset numeric to control the curvature of the series for this grouping.
#' @param curvature_noise numeric multiplier to apply to the standard noise level for the curvature for this grouping.
#'
#' @return timefitteR_Param_List
#' @export
#'
#' @examples
#' x <- timefitteR_Param_List()
timefitteR_Param_List <- function(
    intercept_offset = 0,
    intercept_noise = 1,
    slope_offset = 0,
    slope_noise = 1,
    curvature_offset = 0,
    curvature_noise = 1){

  validate_timefitteR_Param_List(
    new_timefitteR_Param_List(
    as.double(intercept_offset),
    as.double(intercept_noise),
    as.double(slope_offset),
    as.double(slope_noise),
    as.double(curvature_offset),
    as.double(curvature_noise)))
}

#' TimefitteR_Param_List
#'
#' The is.timefitteR_Param_List is used to check if the supplied argument is a correctly formed and likely valid parameter list for use with timefitteR
#'
#' @param x a suspected timefitteR_Param_List object
#'
#' @return LOGICAL
#' @export
#'
#' @examples
#' x <- timefitteR_Param_List()
#' is.timefitteR_Param_List(x)
#' # TRUE
#' x$test <- "something"
#' is.timefitteR_Param_List(x)
#' # FALSE
#'
is.timefitteR_Param_List <- function(x){
  if(!identical(attr(x,"class"),"timefitteR_Param_List")){
    return(FALSE)
  }else if(!identical(x,tryCatch({
    validate_timefitteR_Param_List(x)},error = function(e){
      NULL
    }))){
    return(FALSE)
  }else{
    return(TRUE)
  }
}
