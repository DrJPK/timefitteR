#' An S4 Class that defines a data generation model
#'
#' TimefitteR has the ability to generate semi-deterministic time series data sets. This class holds the set of parameters used to create these data sets.
#'
#' @slot vars Is a list
#' @slot time_params list.
#' @slot conditions_params list.
#' @slot predictors_params list.
#'
#' @include classes_param_model.R
#'
#' @name TimefitteR_Data_model
#' @rdname TimefitteR_Data_model
#' @export
#'
#' @examples
#' x <- new("TimefitteR_Data_Model")
setClass("TimefitteR_Data_Model",
  slots = list(
    vars = setClass("Timefitter_Vars",
                    slots = list(
                      time = "character",
                      conditions = "list",
                      predictors = "list")
                    ),
    time_params = setClass("Timefitter_Seq_Params",
                           slots = list(
                             from = "numeric",
                             length = "numeric",
                             step = "numeric")
                           ),
    base_params = "list",
    conditions_params = "list",
    predictors_params = "list"
  )
)

