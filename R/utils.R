set_names_list <- function(l){
  x <- names(l)
  if(is.null(x)){
    names(l) <- paste0('m_', seq_len(length(l)))
  }else{
    ind <- x == ''
    new_names <- x
    new_names[ind] <- paste0('m_', seq_len(length(l)))[ind]
    names(l) <- new_names
  }
  return(l)
}


#' Gets dates by indexes from modelData
#'
#' @param this modelStrategy
#' @param indexes numeric vector or NULL
#'
#' @return vector of dates
#' @export
#' @rdname getDateByIndex
#' @method getDateByIndex modelStrategy
getDateByIndex.modelStrategy <- function(this, indexes = NULL){
  fun <- function(model, indexes = NULL){
    if(!is.null(indexes)){
      return(index(model$data_margin)[indexes])
    }else{
      return(index(model$data_margin))
    }
  }
  return(fun(getModelD(this), indexes))
}


#' Gets dates by indexes from modelData
#'
#' @param this modelPortfolio
#' @param indexes numeric vector or NULL
#'
#' @return vector of dates
#' @export
#' @rdname getDateByIndex
#' @method getDateByIndex modelPortfolio
getDateByIndex.modelPortfolio <- function(this, indexes = NULL){
  if(is.null(indexes)){
    return(this$thisEnv$backtests[[1]]$results$dates)
  }
  return(this$thisEnv$backtests[[1]]$results$dates[indexes])
}


#' Get modelData
#'
#' @param l list, list of strategies
#' @param ... arguments 
#'
#' @export
getModelD.list <- function(l, ...){
  lapply(l, function(x){
    getModelD(x, ...)
  }) %>%
    set_names(names(l))
}


#' Set modelData
#'
#' @param l list, list of strategies
#' @param ... arguments 
#'
#' @export
setModelD.list <- function(l, data){
  for(i in seq_along(l)){
    if(class(data)[1] == 'modelData'){
      setModelD(l[[i]], data)
    }else{
      setModelD(l[[i]], data[[i]])
    }
  }
}



#' Sets modelData object to modelStrategy object
#'
#' @param this modelStrategy
#' @param x modelData
#'
#' @export
#' @rdname setModelD
#' @method setModelD modelStrategy
setModelD.modelStrategy <- function(this, x, clearBacktests = TRUE){
  e <- this$thisEnv
  e$modelD <- x
  # setCommissionTable(this)
  # setSlippageTable(this)
  if(clearBacktests){
    e$backtests <- list()
  }
  x <- lapply(names(e$paramsets), function(x){
    e$paramsets[[x]]$results <- NULL
    e$paramsets[[x]]$report <- NULL
  })
  processBetaTable(this)
}


#' Gets modelData object from modelStrategy object
#'
#' @param this modelStrategy
#'
#' @export
#' @rdname getModelD
#' @method getModelD modelStrategy
getModelD.modelStrategy <- function(this){
  return(this$thisEnv$modelD)
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



