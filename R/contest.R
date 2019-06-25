#' Get scores from submitted model
#'
#' @param this modelStrategy
#'
#' @return list
#' @export
getScores.modelStrategy <- function(this){
  return(this$thisEnv$scores)
}

#' Get public name of user. It will be defined after you submit this model
#'
#' @param this modelStrategy
#'
#' @return
#' @export
#'
#' @examples
getPublicName.modelStrategy <- function(this){
  if(is.null(this$thisEnv$public_name)){
    message('Submit model and than call this method')
  }else{
    return(this$thisEnv$public_name)
  }
}