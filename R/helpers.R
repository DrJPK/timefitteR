#' An Internal helper function to collect console input
#'
#' Displays a console prompt. Checks if input is of a valid type and returns an object of type
#'
#' @param prompt A string containing a message to be displayed
#' @param check A logical indicating if the input type should be checked. If FALSE a character string will be returned irrespective of the value of rtn
#' @param caseSensitive A logical. TRUE means that input will not be passed to toupper() before comparison and return.
#' @param msg An optional message to print to the console before the prompt
#' @param type one of c("any","character","numeric","list") to define acceptable input types
#'
#' @return an object defined by type
#'
#' @examples
#' x <- getInput("Do you like ice cream?", type="string")
#'

getInput <- function(prompt,check=TRUE,caseSensitive=FALSE,msg=NULL,type="any"){
  ## Should the input be validated? If yes and no conditions supplied then stop
  type <- toupper(substring(type,1,1))
  # Display optional message before prompt
  if(!is.null(msg)){
    cat(paste(msg,"\n"))
  }
  # Display prompt and wait for input
  while(!is.null(x<-readline(prompt = paste(prompt,": ")))){
    # If case sensitive then convert input to upper case
    if(!caseSensitive){
      x <- toupper(x)
    }
    # If not validating the input and accepting any input then return early
    if(!check){
      return(as.character(x))
    }
    if(type %in% c("A","C","N","L")){
      if(type == "N"){
        if(!is.numeric(x)){
          cat("input must be a number\n")
        }else{
          return(as.numeric(x))
        }
      }else if(type == "L"){
        if(!is.list(x)){
          cat("Input must be a list\n")
        }else{
          return(as.list(x))
        }
      }else if(type == "C"){
        if(!is.character(as.character(x))){
          cat("Input must be coercable to a string\n")
        }else{
          return(as.character(x))
        }
      }else{
        if(is.list(x)){
          return(as.character(paste(unlist(x),collapse = '')))
        }else{
         return(as.character(x))
        }
      }
    }else{
      stop(
        "Unknown input type requested."
      )
    }
  }
}


#' An Internal helper function to collect and process console input
#'
#' Displays a console prompt. Checks if input is in valid list (if supplied) else is not in invalid list and returns an object of type rtn.
#'
#' @param prompt A string containing a message to be displayed
#' @param check A logical indicating if the input should be checked. If FALSE a character string will be returned irrespective of the value of rtn
#' @param valid A list containing valid responses
#' @param invalid A list containing invalid responses.
#' @param caseSensitive A logical. TRUE means that input will not be passed to toupper() before comparison and return.
#' @param msg An optional message to print to the console before the prompt
#' @param type one of c("any","character","numeric","param_list") to define acceptable input types
#' @param rtn one of c("string","factor","param_list") to define expected return type
#'
#' @return an object defined by type
#'
#' @examples
#' x <- getConsoleInput("Do you like ice cream?", valid = c("Yes","No"), type="string")
#'
getConsoleInput <- function(prompt,check=TRUE,valid=NULL,invalid=NULL,caseSensitive=FALSE,msg=NULL,type="any",rtn="string"){

}
