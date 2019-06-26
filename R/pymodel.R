#' Set python model
#' 
#' User can create realization of python class Model inherited from BaseModel. All dependencies user should specify in Dockerfile,
#' then with help of addDocker push docker to gitlab.
#'
#' @param this modelStrategy
#' @param pyfile character, path to .py file
#' @param dockername character, name of docker container
#' @param data quote, expression that returns numeric vector or matrix
#' @param pathwise logical, if TRUE then http requests will be send on each iteration
#' @param as character, name of variable that will contains results of model evaluation
#' @param hostname character, name of host.
#' @param modelpath character, path to pretrained model
#' @param update_with_betas logical, by default FALSE. If TRUE then model will be reinit after reinitilization of betas weights
#' @param lookback_init numeric, periods of time for training the model
#' @param lookback_step numeric, periods of time for step of the model
#' @param vector_step logical, if TRUE then model.py file should have ability to take vector to step and output vector after predict 
#' @param port numeric, port for docker container
#' @param lookback numeric, how many periods need to be to evaluate data expression
#'
#' @export
setPyModel.modelStrategy <- function(this,
                                     pyfile,
                                     dockername,
                                     lookback_init,
                                     lookback_step = 0,
                                     lookback = 0,
                                     data = quote(spread),
                                     pathwise = FALSE,
                                     as = 'signal',
                                     hostname = 'localhost',
                                     modelpath = NULL,
                                     update_with_betas = FALSE,
                                     vector_step = FALSE,
                                     args = list(),
                                     port = 4000
){
  if(!file.exists(pyfile)){
    stop('pyfile does not exist')
  }
  if(lookback_init < 0){
    stop('lookback_init should be more or equal than 0')
  }
  lookback_init <- round(lookback_init)
  if(lookback_step < 0){
    stop('lookback_step should be more or equal than 0')
  }
  lookback_step <- round(lookback_step)
  if(!is.null(port)){
    port_str <- paste0(':', port)
  }else{
    port_str <- ''
  }
  this$thisEnv$pymodel <- list(
    pyfile = pyfile,
    dockername = dockername,
    lookback_init = lookback_init,
    lookback_step = lookback_step,
    lookback = lookback,
    data = data,
    pathwise = pathwise,
    hostname = hostname,
    modelpath = modelpath,
    update_with_betas = update_with_betas,
    port = port,
    need_init = TRUE,
    port_str = port_str,
    as = as,
    vector_step = vector_step,
    args = args, 
    id = '1'
  )
}



#' Delete settings of python model
#'
#' @param this modelStrategy
#'
#' @export
deletePyModel.modelStrategy <- function(this){
  this$thisEnv$pymodel <- list()
}

#' Get settings of python model
#'
#' @param this modelStrategy
#'
#' @return list
#' @export
getPyModel.modelStrategy <- function(this){
  this$thisEnv$pymodel
}


#' Set params of python model
#'
#' @param this modelStrategy
#' @param ... params
#'
#' @export
setPyModelParams.modelStrategy <- function(this, ...){
  pm <- getPyModel(this)
  if(length(pm) > 0){
    dots <- list(...)
    this$thisEnv$pymodel <- c(dots, this$thisEnv$pymodel) %>% {.[!duplicated(names(.))]}
    if('port' %in% names(dots)){
      if(!is.null(dots$port)){
        this$thisEnv$pymodel$port_str <- paste0(':', dots$port)
      }else{
        this$thisEnv$pymodel$port_str <- ''
      }
    }
  }
}


#' Upload python model to server
#'
#' @param this modelStrategy
#' @param session ssh_session
uploadPyModel.modelStrategy <- function(this, session){
  if(missing(session)){
    session <- .env[['session']]
  }
  pymodel <- getPyModel(this)
  if(length(pymodel) > 0){
    if(!all(c('pyfile', 'dockername') %in% names(pymodel))){
      stop('Please, define following arguments in pymodel: pyfile, dockername. And you can define modelpath argument
           for path to pretrained model')
    }
    temp <- tempdir()
    if(!dir.exists(temp)){
      dir.create(temp)
    }
    file_path <- file.path(temp, 'archive.zip')
    files <- character()
    if(file.exists(pymodel[['pyfile']])){
      file.copy(pymodel[['pyfile']], file.path(temp, 'model.py'), overwrite = TRUE)
      files <- c(files, file.path(temp, 'model.py'))
    }else{
      stop('pyfile should be existing file')
    }
    if('modelpath' %in% names(pymodel) && !is.null(pymodel[['modelpath']])){
      if(file.exists(pymodel[['modelpath']])){
        files <- c(files, pymodel[['modelpath']])
      }else{
        stop('modelpath should be existing file')
      }
    }
    utils::zip(file_path, files = files, flags = '-j9Xq')
    capture.output(ssh::scp_upload(session, file_path))
  }
}


#' Upload python model to server
#'
#' @param this modelStrategy
#' @param session ssh_session
uploadPyModel.environment <- function(this, session){
  
}


#' Upload python model to server
#'
#' @param this modelStrategy
#' @param session ssh_session
uploadPyModel.modelPortfolio <- function(this, session){
  
}

