#' Test condition
#'
#' Test a condition and returns the according parameter
#'
#' This function allows to test a condition and get the appropriate 
#' output when TRUE/FALSE with avoiding endless if then else loops 
#' in the code.
#'
#' @param condition Condition that needs to be checked
#' @param val_true Return value when condition is true
#' @param val_false Return value when condition is false
#' 
#' @author Yves R. Sagaert
#'
#'
#' @return The second or third input parameter based on the condition
#' @export
#'
#' @examples
#' condition(5<10,val_true="5 is smaller than 10",val_false="5 is bigger than 10")
#'
condition <- function(condition, val_true, val_false){
  if (condition){
    return(val_true)
  } else {
    return(val_false)
  }
}