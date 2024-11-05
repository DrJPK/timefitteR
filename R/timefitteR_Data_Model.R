#' Create New TimefitteR Data Model
#'
#' The timefitter functions to create synthetic data sets expect to find a certain defined model. This set of functions builds that model. The model general form is:
#' Model:
#' -->vars
#'   -->timevariable
#'   -->time_params
#'     -->from
#'     -->length
#'     -->step
#'   -->conditions
#'   -->predictors
#'     -->predictor 1
#'     -->predictor 2
#'     ....
#` -->base_params
#`   -->timefitteR_Param_List
#` -->condition_params
#`   -->condition level 2
#`     -->timefitteR_Param_List
#`   -->condition level ...
#` -->predictor_params
#`   -->predictor 1
#'     -->predictor 1 level 2
#'       -->timefitteR_Param_List
#'     -->predictor 1 level 3 ...
#'   -->predictor 2
#'     --> ...
#'
#' @param time Name of the variable to be used to represent timepoints
#' @param conditions A factor variable defining comparison groups e.g., factor(c("Control","Intervention), levels = c("Control","Intervention"))
#' @param predictors A list of n factors defining the variables to be used as predictors in exploring the model
#' @param base_params A timefitteR Param List that defines the bases values used to generate the data set for the `[1]` levels of each of the n predictor factors
#' @param condition_params A list of timefitter_Param_Lists that define the additional parameters for conditions other than the base level
#' @param predictor_params A list of lists of timefitter_Param_Lists that define the additional parameters for conditions other than the base level
#' @param from The value of the first time point (usually 0)
#' @param length The number of time point steps (default 10)
#' @param step The step size between time points (usually 1)
#'
#' @return a timefitteR_Data_Model class
#' @export
#'
#' @examples
#' y<-new_timefitteR_Data_Model(Timer,conditions = factor(c("Control","Intervention","test")), predictors = list("Gender" = factor(c("Boy","Girl")),"SES" = factor(c("Low","High"),levels = c("Low","High"))), condition_params = list("x"=timefitteR_Param_List(1,2,3,4,5,6),"y"=timefitteR_Param_List(4,5,6,7,8,9)))
#' y
#'

new_timefitteR_Data_Model <- function(time,
                                      conditions = factor(),
                                      predictors = list(),
                                      base_params = timefitteR_Param_List(),
                                      condition_params = list(),
                                      predictor_params = list(),
                                      from = 0L,
                                      length = 10L,
                                      step = 1L) {
  ##Check Required Parameter Types
  stopifnot(
    !missing(time),
    is.integer(from),
    is.integer(length),
    is.integer(step),
    is.factor(conditions),
    is.timefitteR_Param_List(base_params),
    is.list(condition_params),
    is.list(predictor_params)
  )
  ##Check that optional parameters of of correct types
  if (is.list(predictors) & length(predictors) == 0) {
    cli::cli_alert_info(
      "No predictors supplied. If any predictor_params were supplied they will be ignored. Continuing ..."
    )
  } else if (!all(lapply(predictors, is.factor))) {
    stop("Predictors must be a list of factors")
  }

  #Clean the Condition Parameters Up
  condition_params <- clean_param_list(condition_params,levels(conditions),"conditions")

  #Now deal with the predictors

  if(length(predictors)==0){
    tmp_predictor_params <- NULL
  }else{
    tmp_predictor_params <- list()
    for(i in 1:length(predictors)){
      if(i<length(predictor_params)){
        x=levels(predictor_params[[i]])
      }else{
        x = list()
      }
      tmp_predictor_params <- append(tmp_predictor_params,list(clean_param_list(x,levels(predictors[[i]]),as.character(names(predictors[i])))))

    }
    names(tmp_predictor_params) <- as.character(names(predictors))
  }


  ##Build Standard Variable Lists
  time_params = list("from" = from,
                     "length" = length,
                     "step" = step)

  vars = list(
    "timevariable" = substitute(time),
    "time_params" = time_params,
    "conditions" = conditions,
    "predictors" = predictors
  )

  ##Create and Output Structure
  structure(
    list(
      "vars" = vars,
      "base_params" = base_params,
      "condition_params" = condition_params,
      "predictor_params" = tmp_predictor_params
    ),
    class = "timefitteR_Data_Model"
  )
}


#' Clean TimefitteR Parameter Lists
#'
#' A function for internal use only
#'
#' @param p A list of timefitteR_Param_List objects
#' @param l A vector containing the levels of the factor
#' @param n A  string containing the name of the factor
#'
#' @return A list
#'
clean_param_list <-function(p, l, n){

  ##Handle condition parameters
  if (is.list(p)) {
    if (length(p) == 0) {
      if (length(l) == 1) {
        cli::cli_alert_info(
          paste("No parameters supplied for",n,"and a variable with only one level was found. This may be OK, but check this is what is intended!")
        )
      } else{
        cli::cli_alert_info(
          paste("No parameters supplied for",n,"Default parameters have been set for all levels of the variable.")
        )
        for (i in 2:length(l)) {
          p <- append(p, list(timefitteR_Param_List()))
          names(p)[i - 1] <- as.character(l[i])
        }
      }
    }else{
      ##condition_params passed in. Are they appropriate?
      if(!all(lapply(p, is.timefitteR_Param_List))){
        stop(
          paste(n,"has been passed into the function but its components are not all timefitteR_Param_List objects")
        )
      }else{
        if(length(p)<length(l)-1){
          ##Not enough params
          for (i in (length(p)+1):(length(l)-1)) {
            p <- append(p, list(timefitteR_Param_List()))
          }
          names(p) <- as.character(l[2:length(l)])
          cli::cli_alert_info(
            paste("Not enough parameters supplied for all levels of the variable. Default parameters have been set for all additional levels of the ",n," variable.")
          )
        }else if(length(p)>length(l)-1){
          ##Too many params
          p <- p[1:(length(f)-1)]
          names(p) <- as.character(l[2:length(l)])
          cli::cli_alert_info(
            "Too many parameters supplied for all levels. Additional parameters have been trimmed."
          )
        }else{
          ##Just right.
          names(p) <- as.character(l[2:length(l)])
        }
      }
    }
  } else{
    stop(
      paste("If",n,"is passed, then it must be a list of timefitteR_Param_List objects")
    )
  }
  return(p)
}
