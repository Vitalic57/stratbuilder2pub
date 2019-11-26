#' Copy modelStrategy object
#'
#' @param this modelStrategy
#' @param clone_data logical, if TRUE then clone modelData object
#' @param clone_backtests logical, if TRUE then all backtests will be cloned
#' @param ... params
#'
#' @return modelStrategy
#' @export
#' @rdname cloneStrategy
#' @method cloneStrategy modelStrategy
cloneStrategy.modelStrategy <- function(this, clone_data = FALSE, clone_backtests = FALSE, ...){
  tmp <- tempfile()
  if(!clone_data){
    data <- getModelD(this)
    this$thisEnv$modelD <- NULL
  }
  if(!clone_backtests){
    backtests <- this$thisEnv$backtests
    this$thisEnv$backtests <- list()
  }
  saveRDS(this, tmp)
  this_cloned <- readRDS(tmp)
  if(!clone_data){
    this$thisEnv$modelD <- data
    this_cloned$thisEnv$modelD <- data
  }  
  if(!clone_backtests){
    this$thisEnv$backtests <- backtests
  }
  parent.env(this_cloned$thisEnv) <- parent.env(this$thisEnv)
  return(this_cloned)
}


#' @return modelStrategy
#' @export
cloneStrategy.list <- function(this, ...){
  res <- list()
  for(i in 1:length(this)){
    res[[i]] <- cloneStrategy(this[[i]], ...)
  }
  return(res)
}

#' @return modelPortfolio
#' @export
#' @rdname cloneStrategy
#' @method cloneStrategy modelPortfolio
cloneStrategy.modelPortfolio <- function(this, ...){
  e <-  this$thisEnv %>%
    ls %>%
    setdiff(., c('backtests','modelD', 'models')) %>%
    mget(.,envir = this$thisEnv) %>%
    as.environment
  e$me$thisEnv <- e
  e$thisEnv <- e
  e$thisEnv$models <- cloneStrategy.list(this$thisEnv$models, ...)
  return(e$me)
}


#' @export
#' @rdname cloneStrategy
cloneStrategy <- function(this,...){
  UseMethod('cloneStrategy', this)
}
