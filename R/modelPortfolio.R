#' creates modelPortfolio object
#'
#' @return modelPortfolio object
#' @export
#'
modelPortfolio <- function(...){
  thisEnv <- environment()
  models <- list(...)
  if(length(models) == 1 && is.list(models[[1]]) && length(models[[1]]) > 1){
    models <- models[[1]]
  }
  if(is.null(names(models))){
    names(models) <- paste0('x', seq_len(length(models)))
  }else{
    if(any(names(models) == '')){
      ind <- which(names(models) == '')
      names(models)[ind] <- paste0('x', ind)
    }
  }
  backtests <- list()
  me <- list(thisEnv = thisEnv)
  
  
  ## Set the name for the class
  class(me) <- c("modelPortfolio")
  
  ## Define the value of the list within the current environment.
  assign('this', me, envir=thisEnv)
  return(me)
}



#' Simulate portfolio of strategies
#'
#' @param this modelPortfolio
#' @param ... arguments to perform function
#'
#' @export
performServer.modelPortfolio <- function(this, ...){
  this$thisEnv$data_changed <- TRUE#any(sapply(this$thisEnv$models, function(x) x$thisEnv$data_changed)) 
  x <- performServer.modelStrategy(this, ...)
  # for(model in this$thisEnv$models){
  #   model$thisEnv$data_changed <- FALSE
  #   model$thisEnv$user_beta_table_changed <- FALSE
  # }
  return(x)
}


#' Apply paramset to list of strategies and summarize results
#'
#' @param this modelPortfolio
#' @param ... arguments to perform function
#'
#' @export
applyParamsetServer.modelPortfolio <- function(this, ...){
  this$thisEnv$data_changed <- TRUE # any(sapply(this$thisEnv$models, function(x) x$thisEnv$data_changed)) 
  x <- applyParamsetServer.modelStrategy(this, ...)
  # for(model in this$thisEnv$models){
  #   model$thisEnv$data_changed <- FALSE
  #   model$thisEnv$user_beta_table_changed <- FALSE
  # }
  return(x)
}




#' Get modelData
#'
#' @param this modelPortfolio
#'
#' @export
getModelD.modelPortfolio <- function(this){
  getModelD.list(this$thisEnv$models)
}


#' Set modelData
#'
#' @param this modelPortfolio
#'
#' @export
setModelD.modelPortfolio <- function(this, data){
  setModelD.list(this$thisEnv$models, data)
}





#' Return sum money of models inside portfolio
#'
#' @param this modelPortfolio
#'
#' @return numeric
#' @export
getMoney.modelPortfolio <- function(this){
  s <- 0
  for(model in this$thisEnv$models){
    s <- s + getMoney(model)
  }
  return(s)
}




#' Add distribution to list of models
#' 
#' This method add the same distribution to each model in list
#'
#' @param this modelPortfolio
#' @param ... params for addDistribution
#'
#' @export
addDistribution.modelPortfolio <- function(this, ...){
  addDistribution.list(this$thisEnv$models, ...)
}



#' Add distributions' constraint to list of models
#' 
#' This method add the same distributions' constraint to each model in list
#'
#' @param this modelPortfolio
#' @param ... params for addDistribution
#'
#' @export
addDistributionConstraint.modelPortfolio <- function(this, ...){
  addDistributionConstraint.list(this$thisEnv$models, ...)
}



#' Removes paramset from each strategy in list of models
#'
#' @param this modelPortfolio
#' @param ... params for addDistribution
#'
#' @export
deleteParamset.modelPortfolio <- function(this, ...){
  deleteParamset.list(this$thisEnv$models, ...)
}





