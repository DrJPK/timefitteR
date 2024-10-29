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
  condition_params <- clean_param_list(condition_params,conditions)

  #Now deal with the predictors

  if(length(predictors)==0){
    tmp_predictor_params <- NULL
  }else{
    tmp_predictor_params <- list()
    for(i in 1:length(predictors)){
      print(as.name(predictors[i]))
      tmp_predictor_params <- append(tmp_predictor_params,
                                   clean_param_list(ifelse(i<length(predictor_params),
                                                           predictor_params[[i]],
                                                           list()),
                                                    predictors[i]))
    }
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
#' @param f A factor with at least 1 level
#'
#' @return A list
#'
clean_param_list <-function(p,f){
  ##Handle condition parameters
  if (is.list(p)) {
    n = quote(f)
    if (length(p) == 0) {
      if (length(levels(f)) == 1) {
        cli::cli_alert_info(
          paste("No parameters supplied for",n,"and a variable with only one level was found. This may be OK, but check this is what is intended!")
        )
      } else{
        cli::cli_alert_info(
          paste("No parameters supplied for",n,"Default parameters have been set for all levels of the variable.")
        )
        for (i in 2:length(levels(f))) {
          p <- append(p, list(timefitteR_Param_List()))
          names(p)[i - 1] <- as.character(levels(f)[i])
        }
      }
    }else{
      ##condition_params passed in. Are they appropriate?
      if(!all(lapply(p, is.timefitteR_Param_List))){
        stop(
          paste(n,"has been passed into the function but its components are not all timefitteR_Param_List objects")
        )
      }else{
        if(length(p)<length(levels(f))-1){
          ##Not enough params
          for (i in (length(p)+1):(length(levels(f))-1)) {
            p <- append(p, list(timefitteR_Param_List()))
          }
          names(p) <- as.character(levels(f)[2:length(levels(f))])
          cli::cli_alert_info(
            paste("Not enough parameters supplied for all levels of the variable. Default parameters have been set for all additional levels of the ",n," variable.")
          )
        }else if(length(p)>length(levels(f))-1){
          ##Too many params
          p <- p[1:(length(f)-1)]
          names(p) <- as.character(levels(f)[2:length(levels(f))])
          cli::cli_alert_info(
            "Too many parameters supplied for all levels. Additional parameters have been trimmed."
          )
        }else{
          ##Just right.
          names(p) <- as.character(levels(f)[2:length(levels(f))])
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
