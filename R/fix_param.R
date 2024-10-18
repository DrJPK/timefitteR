#' Fix Parameters Helper
#'
#' This helper function is used by [generate_data_set] to ensure that the length of the parameter vectors passed to the model are of n-length. Parameter vectors of the wrong length are trimmed or padded with the last value in order to work.
#'
#' @param param a vector of parameter values or an object that can be coerced to an atomic vector
#' @param n the length of the vector to be returned
#'
#' @return an atomic vector of n-length
#'
#' @examples
#' fix_param(param=c(1,2,3,4),n=2)
#'

fix_param <- function(param, n){
  ## Check if it is something we can work with
  if(!(is.atomic(param) || is.list(param) || is.null(param))){
    cli::cli_alert_danger("Parameter could not be coerced to a vector")
    stop()
  }
  ## get param into the form of a 1d vector
  if(is.list(param)){
    x <- unlist(param)
  }else if(is.data.frame(param)){
    x <- as.vector(param[,1])
  }else{
    x <- as.vector(param)
  }
  names(x) <- NULL
  ## get vector length for multiple comparison
  l <- length(x)
  ## Case conditions
  if(l==n){
    return(x)
  }else if(l<n){
    #extend x to n elements by repeating the last element
    for(i in l:n){
      x[i] <- x[l]
    }
    return(x)
  }else{
    return(x[1:n])
  }
}
