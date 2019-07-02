#' Get scores from submitted model
#'
#' @param this modelStrategy
#'
#' @return list
#' @export
#' @rdname getScores
getScores <- function(this){
  UseMethod('getScores', this)
}

#' @export
#' @rdname getScores
#' @method getScores modelStrategy
getScores.modelStrategy <- function(this){
  return(this$thisEnv$scores)
}

#' Get public name of user. It will be defined after you submit this model
#'
#' @param this modelStrategy
#'
#' @return
#' @export
#' @rdname getPublicName
getPublicName <- function(this){
  UseMethod('getPublicName', this)
}

#' @export
#' @rdname getPublicName
#' @method getPublicName modelStrategy
getPublicName.modelStrategy <- function(this){
  if(is.null(this$thisEnv$public_name)){
    message('Submit model and than call this method')
  }else{
    return(this$thisEnv$public_name)
  }
}