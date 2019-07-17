#' Do backtest of combinations from paramset
#'
#' @param this model
#' @param paramset.label charater, name of paramset
#' @param nsamples numeric, how many backtests to do
#' @param session ssh_session
#' @param verbose logical, if true then logs will be printed
#' @param ... additional arguments (start_date, end_date)
#'
#' @return data.frame, reports of simulations
#' @export
#' @rdname applyParamsetServer
applyParamsetServer <- function(this,
                                paramset.label, 
                                session,
                                nsamples = 100,
                                verbose=FALSE, 
                                ...){
  UseMethod('applyParamsetServer', this)
}

#' @export
#' @rdname applyParamsetServer
#' @method applyParamsetServer modelStrategy
applyParamsetServer.modelStrategy <- function(this, 
                                              paramset.label, 
                                              session,
                                              nsamples = 100,
                                              verbose=FALSE, 
                                              ...){
  if(missing(session)){
    session <- .env[['session']]
  }
  this$thisEnv$user_args <- c(list(action = 'applyParamset',
                                   nsamples = nsamples),
                              list(...))
  if(!missing(paramset.label)){
    this$thisEnv$user_args <- c(this$thisEnv$user_args, list(paramset.label = paramset.label))
  }
  
  this[['settings']] <- .settings
  # upload part  ---------------------------
  this[['version']] <- packageVersion('stratbuilder2pub')
  send_rdata(session, this)
  if(!('reload' %in% names(.settings) && .settings[['reload']])){
    this$thisEnv$data_changed <- FALSE
    this$thisEnv$user_beta_table_changed <- FALSE
  }
  Sys.sleep(3)
  get_results(this, session, 'strategy', verbose)
}







#' @return list, list of data.frames (reports of simulations)
#' @export
#' @rdname applyParamsetServer
#' @method applyParamsetServer list
applyParamsetServer.list <- function(this, 
                                    paramset.label,
                                    session,
                                    nsamples = 100,
                                    verbose=FALSE,
                                    ...){
  if(missing(session)){
    session <- .env[['session']]
  }
  if(missing(paramset.label)){
    paramset.label <- names(this[[1]]$thisEnv$paramsets)[1]
  }
  this <- set_names_list(this)
  e <- new.env()
  e[['strategies']] <- this
  e[['user_args']] <- c(list(action = 'applyParamset',
                           nsamples = nsamples, 
                           paramset.label = paramset.label),
                        list(...))
  e[['settings']] <- .settings
  e[['data_changed']] <- TRUE#any(sapply(l, function(x) x$thisEnv$data_changed))
  e[['version']] <- packageVersion('stratbuilder2pub')
  send_rdata(session, e)
  
  Sys.sleep(3)
  get_results(this, session, 'strategy', verbose)
}








#' Send your strategy to server for backtesting.
#' 
#' All results could be seen in your folder on server.
#'
#' @param this model
#' @param session ssh_session
#' @param ... additional arguments:
#' start_date -- from this date backtest will be started
#' end_date -- at this date backtest will be ended. Max end_date is Sys.Date() - 365
#' paramset.label -- character, name of paramset
#' paramset.index -- numeric, index of combination of params to backtest, by defualt current strategy will be backtested
#' @param verbose logical, if true then logs will be printed
#' 
#' @return data.frame, report of strategy
#' @export
#' @rdname performServer
performServer <- function(this, 
                          session,
                          verbose=FALSE,
                          ...){
  UseMethod('performServer', this)
}


#' @export
#' @rdname performServer
#' @method performServer modelStrategy 
performServer.modelStrategy <- function(this, 
                                        session,
                                        verbose=FALSE,
                                        ...){
  if(missing(session)){
    session <- .env[['session']]
  }
  this$thisEnv$user_args <- c(list(...), list(action = 'perform'))
  if ('paramset.index' %in% names(this$thisEnv$user_args)){
    this$thisEnv$user_args[['paramset.index']] <- unlist(this$thisEnv$user_args['paramset.index'], use.names=FALSE)
  }
  if('paramset.index' %in% names(this$thisEnv$user_args) && !'paramset.label' %in% names(this$thisEnv$user_args)){
    this$thisEnv$user_args[['paramset.label']] <- names(this$thisEnv$paramsets)[1]
  }
  this[['settings']] <- .settings
  this[['version']] <- packageVersion('stratbuilder2pub')
  if(verbose){
    cat('Before sending data\n')
  }
  send_rdata(session, this)
  if(verbose){
    cat('After sending data\n')
  }
  if(!('reload' %in% names(.settings) && .settings[['reload']])){
    this$thisEnv$data_changed <- FALSE
    this$thisEnv$user_beta_table_changed <- FALSE
  }
  Sys.sleep(0.5)
  if(verbose){
    cat('Before getting results\n')
  }
  get_results(this, session, reports=NULL, verbose)

}



#' @return list
#' @export
#' @rdname performServer
#' @method performServer list
performServer.list <- function(this, session, verbose=FALSE, ...){
  print(1)
  if(missing(session)){
    session <- .env[['session']]
  }
  this <- set_names_list(this)
  e <- new.env()
  e[['strategies']] <- this
  e[['user_args']] <- c(list(...), list(action = 'perform'))
  if ('paramset.index' %in% names(this$thisEnv$user_args)){
    this$thisEnv$user_args[['paramset.index']] <- unlist(this$thisEnv$user_args['paramset.index'], use.names=FALSE)
  }
  if('paramset.index' %in% names(e[['user_args']]) && !'paramset.label' %in% names(e[['user_args']])){
    tryCatch({
      print(names(this$thisEnv$paramsets)[1])
      e[['user_args']][['paramset.label']] <- as.numeric(names(this[[1]]$thisEnv$paramsets)[1])
    }, error = function(e){
      stop('Please, define paramset.label argument')
    })
    if(is.null(names(this[[1]]$thisEnv$paramsets)[1])){
      stop('Please, define paramset.label argument')
    }
  }
  # e[['user_args']] <- c(list(action = 'perform', reports = reports))
  e[['settings']] <- .settings
  e[['data_changed']] <- TRUE#any(sapply(l, function(x) x$thisEnv$data_changed)) 
  e[['version']] <- packageVersion('stratbuilder2pub')
  send_rdata(session, e, verbose)
  # for(this in l){
  #   this$thisEnv$data_changed <- FALSE
  #   this$thisEnv$user_beta_table_changed <- FALSE
  # }
  Sys.sleep(0.5)
  get_results(this, session, reports=NULL, verbose)
}

send_rdata <- function(session, obj, verbose=FALSE){
  uploadPyModel(obj, session)
  file_path <- file.path(tempdir(), 'file.RData')
  saveRDS(obj, file_path, version = rds_version)
  tryCatch({
    capture.output(ssh::scp_upload(session, file_path))
  }, error = function(e){
    #print(e)
  })
  if(verbose){
    cat('data uploaded\n')
  }
  
  capture.output(ssh::ssh_exec_wait(session, 'cat file.RData > .RData'))
  if(verbose){
    cat('File renamed on server\n')
  }
  file.remove(file_path)
  if(verbose){
    cat('Temporary file removed\n')
  }
}

#' Download files from the server
#' 
#' @param session ssh_session
#' @param file character, name of a file in the working directory of user
#' @param to character, path to folder where to upload file
#' @param ... params
#'
#' @export
scp_download <- function(session, file, to = '.', ...){
  info <- ssh_info(session)
  pwd <- capture.output(ssh::ssh_exec_wait(session, 'pwd'))[1]
  if(!is.null(info$keyfile)){
    s <- paste0('scp ', ' -i ', info$keyfile, ' -l 8192 ',   info$user, '@', info$host, ':', pwd, '/', file, ' ', to)
  }else{
    s <- paste0('scp ', ' -l 8192 ',  info$user, '@', info$host, ':', pwd, '/', file, ' ', to)
  }
  if(Sys.info()['sysname'] == 'Windows'){
    shell(s)
  }else{
    system(s)
  }
}

#' Upload files to server
#'
#' @param session ssh_session
#' @param file character, path to file
#' @param to character, name of folder to upload
#' @param ... params
#'
#' @export
scp_upload <- function(session, file, to = '.', ...){
  info <- ssh_info(session)
  pwd <- capture.output(ssh::ssh_exec_wait(session, 'pwd'))[1]
  if(!is.null(info$keyfile)){
    s <- paste0('scp ', ' -i ', info$keyfile, ' -l 8192 ', file, ' ', info$user, '@', info$host, ':', pwd, '/', to)
  }else{
    s <- paste0('scp ', ' -l 8192 ',  info$user, '@', info$host, ':', pwd, '/', file, ' ', to)
  }
  if(Sys.info()['sysname'] == 'Windows'){
    shell(s)
  }else{
    system(s)
  }
}

#' Get info about the session
#'
#' @param session ssh_session
#'
#' @export
ssh_info <- function(session){
  info <- ssh::ssh_info(session)
  info$keyfile <- .env[['keyfile']]
  return(info)
}


.env <- new.env()

#' Connect to server
#'
#' @param host character, name of host
#'
#' @param keyfile character, path to key
#' @param passwd askpass::askpass
#' @param verbose logical
#'
#' @export
ssh_connect <- function(host, keyfile = NULL, passwd = askpass::askpass, verbose = FALSE){
  .env[['keyfile']] <- keyfile
  .env[['session']] <- ssh::ssh_connect(host=host, keyfile = keyfile, passwd = passwd, verbose = verbose)
  return(.env[['session']])
}



# get_results <- function(session, reports, verbose=FALSE){
# 
#   # wait for results--------------------------
#   files_path <- file.path(tempdir())
#   
#   vec_cond <- logical(0)
#   vec_names <- c(pnl = 'pnl.png', strategy = 'report.RDS', calendar = 'report2.RDS', trades = 'trades.RDS')
#   vec_cond['pnl'] <- 'plot' %in% reports
#   vec_cond['strategy'] <-  'strategy' %in% reports
#   vec_cond['calendar'] <-  'calendar' %in% reports
#   vec_cond['trades'] <-  'trades' %in% reports
#   t <- Sys.time()
#   if(verbose){
#     cat('Before cycle of getting results\n')
#   }
#   while(TRUE){
#     x <- capture.output(ssh::ssh_exec_wait(session, 'ls ~/last_results'))
#     res <- any(vec_names[vec_cond] %in% x)
#     Sys.sleep(1)
#     if(verbose && Sys.time() - t > 10){
#       cat('simulation in progress\n')
#       cat('current data in last results:')
#       cat(capture.output(ssh::ssh_exec_wait(session, 'ls ~/last_results')))
#       cat('\n')
#       t <- Sys.time()
#     }
#     if(res){
#       break
#     }
#   }
#   if(verbose){
#     cat('Results got\n')
#   }
#   # available results
#   x <- capture.output(ssh::ssh_exec_wait(session, 'ls ~/last_results'))
#   vec_avail <- vec_names[vec_cond] %in% x
#   names(vec_avail) <- names(vec_names[vec_cond])
#   
#   if(verbose){
#     cat('Before downloading results\n')
#   }
#   # download results -------------------
#   if(verbose){
#     print('vec_names:')
#     print(vec_names)
#     print('vec_cond:')
#     print(vec_cond)
#     print('vec_avail:')
#     print(vec_avail)
#     print('files_path:')
#     print(files_path)
#     
#   }
#   
#   x <- sapply(seq_along(vec_names), function(i){
#     x <- names(vec_names)[i]
#     if(vec_cond[x] && vec_avail[x]){
#       if(verbose){
#         print(paste0('try to download: ', paste0('last_results/', vec_names[x])))
#       }
#       scp_download(session, paste0('last_results/', vec_names[x]), files_path, verbose = TRUE)
#       if(verbose){
#         print('downloaded')
#       }
#     }
#   })
#   if(verbose){
#     cat('Files of results downloaded\n')
#   }
#   
#   # show results ----------------------------------------
#   results <- list()
#   x <- sapply(2:4, function(i){
#     x <- names(vec_names)[i]
#     if(vec_cond[x] && vec_avail[x]){
#       txt_path <- file.path(files_path, vec_names[x])
#       results[[length(results) + 1]] <<- readRDS(txt_path)
#       file.remove(txt_path)
#     }
#   })
#   if(verbose){
#     cat('Reports downloaded\n')
#   }
#   
#   if(vec_cond['pnl'] && vec_avail['pnl']){
#     png_path <- file.path(files_path, 'pnl.png')
#     # image <- imager::load.image(png_path)
#     # imager:::plot.cimg(image, axes = FALSE)
#     img <- png::readPNG(png_path)
#     grid::grid.raster(img)
#     file.remove(png_path)
#     Sys.sleep(1)
#     if(verbose){
#       cat('Image downloaded')
#     }
#   }
#   
#   return(results)
# }


get_results <- function(last_model, session, reports=NULL, verbose=FALSE){
  Sys.sleep(0.5)
  reports <- 'strategy'
  # wait for results--------------------------
  files_path <- file.path(tempdir())
  
  vec_cond <- logical(0)
  vec_names <- c(strategy = 'report.RDS')
  vec_cond['strategy'] <-  TRUE
  t <- Sys.time()
  if(verbose){
    cat('Before cycle of getting results\n')
  }
  while(TRUE){
    x <- capture.output(ssh::ssh_exec_wait(session, 'ls ~/last_results'))
    res <- any(vec_names[vec_cond] %in% x)
    Sys.sleep(1)
    if(verbose && Sys.time() - t > 10){
      cat('simulation in progress\n')
      cat('current data in last results:')
      cat(capture.output(ssh::ssh_exec_wait(session, 'ls ~/last_results')))
      cat('\n')
      t <- Sys.time()
    }
    if(res){
      break
    }
  }
  if(verbose){
    cat('Results have been gotten\n')
  }
  # available results
  x <- capture.output(ssh::ssh_exec_wait(session, 'ls ~/last_results'))
  vec_avail <- vec_names[vec_cond] %in% x
  names(vec_avail) <- names(vec_names[vec_cond])
  
  if(verbose){
    cat('Before downloading results\n')
  }
  # download results -------------------
  if(verbose){
    print('vec_names:')
    print(vec_names)
    print('vec_cond:')
    print(vec_cond)
    print('vec_avail:')
    print(vec_avail)
    print('files_path:')
    print(files_path)
  }
  
  x <- sapply(seq_along(vec_names), function(i){
    x <- names(vec_names)[i]
    if(vec_cond[x] && vec_avail[x]){
      if(verbose){
        print(paste0('try to download: ', paste0('last_results/', vec_names[x])))
      }
      ssh::scp_download(session, paste0('~/last_results/', vec_names[x]), files_path, verbose = FALSE)
      if(verbose){
        print('downloaded')
      }
    }
  })
  if(verbose){
    cat('Files of results downloaded\n')
  }
  
  # show results ----------------------------------------
  txt_path <- file.path(files_path, 'report.RDS')
  res <- readRDS(txt_path)
  file.remove(txt_path)
  if(verbose){
    cat('Reports downloaded\n')
  }
  
  if(res == 'OK'){
    ssh::scp_download(session, paste0('~/last_results/model.RData'), files_path, verbose = FALSE)
    Sys.sleep(1)
    model <- suppressWarnings(readRDS(file.path(files_path, 'model.RData')))
    if(class(last_model)[1] == 'modelStrategy' && class(model)[1] == 'list'){
      return(model)
    }else{
      installModel(last_model, model)
      invisible(NULL)
    }
  }else{
    return(res)
  }
}



installModel <- function(this, target){
  if(class(this)[1] == 'modelStrategy'){
    for(n in setdiff(ls(target$thisEnv, all.names=TRUE), c('me', 'thisEnv'))) assign(n, get(n, target$thisEnv), this$thisEnv)
  }else if(class(this)[1] == 'list'){
    for(i in seq_along(this)){
      for(n in setdiff(ls(target[[i]]$thisEnv, all.names=TRUE), c('me', 'thisEnv'))) assign(n, get(n, target[[i]]$thisEnv), this[[i]]$thisEnv)
    }
  }else if(class(this)[1] == 'modelPortfolio'){
    for(i in seq_along(this$thisEnv$models)){
      for(n in setdiff(ls(target$thisEnv$models[[i]]$thisEnv, all.names=TRUE), c('me', 'thisEnv'))) 
        assign(n, get(n, target$thisEnv$models[[i]]$thisEnv), this$thisEnv$models[[i]]$thisEnv)
    }
    for(n in setdiff(ls(target$thisEnv, all.names=TRUE), c('me', 'thisEnv', 'models'))) assign(n, get(n, target$thisEnv), this$thisEnv)
  }
}


#' Select data to backtest.
#'
#' @param this modelStrategy
#' @param l list or xts, list can contain xts in each cell or 
#' should contain following fields:
#' 1. dataset -- name of dataset, now only "Russia" is available
#' 2. period -- periodicity of data. It is one of c('hour', 'day').
#' 3. assets -- character vector of names of assets from dataset
#' 4. time -- numeric, if period == 'day', then time should be specified. Time here is hour.
#' For example, if time == 19, then data will be at 19:00 every business day. This field works only in
#' Russia dataset. Xts -- table of timeserieses of instruments.
#' if l is xts, then each column should be close price for instruments.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' setUserData(this, list(
#'    dataset = 'Russia',
#'    period = 'day',
#'    assets = c('GAZP', 'LKOH'),
#'    time = 19
#' ))
#' 
#' x <- xts(cumsum(rnorm(1000)) + 300, Sys.Date()  + 1:1000)
#' setUserData(this, x)
#' 
#' x <- xts(cumsum(rnorm(1000)) + 300, Sys.Date()  + 1:1000)
#' y <- xts(cumsum(rnorm(1000)) + 300, Sys.Date()  + 1:1000)
#' setUserData(this, cbind(x, y))
#' 
#' x <- quantmod::getSymbols("AAPL", from = Sys.Date() - 365)
#' x <- quantmod::getSymbols("MSFT", from = Sys.Date() - 365)
#' setUserData(this, cbind(x, y))
#' }
#' @rdname setUserData
setUserData <- function(this, l){
  UseMethod('setUserData', this)
}


#' @export
#' @rdname setUserData
#' @method setUserData modelStrategy
setUserData.modelStrategy <- function(this, l){
  if(is.list(l) && !xts::is.xts(l[[1]])){
    if(!all(c('dataset') %in% names(l))){
      stop('l must contain dataset field')
    }
    if(!'period' %in% names(l)){
      l[['period']] <- 'day'
    }
    if(l[['period']] == 'day'){
      if(!('time' %in% names(l))){
        l[['time']] <- 19
      }else if(!is.numeric(l[['time']])){
        stop('Time must be a numeric 11-23')
      }else{
        l[['time']] <-  min(max(round(l[['time']]), 11), 23)
      }
    }else if(l[['period']] != 'hour'){
      stop('Period can be only hour or day')
    }
    if(tolower(l[['dataset']]) %in% names(datasets)){
      stop(paste0('Available datasets: ', paste(names(datasets), collapse = ', ')))
    }
    if('assets' %in% names(l)){
      if(!all(l[['assets']] %in% datasets[[l[['dataset']]]])){
        stop('Names of assets must be from your selected dataset. Please check names in stratbuilder2pub:::datasets.')
      }
    }
    this$thisEnv$data_from_user <- l
  }else if(is.list(l) && xts::is.xts(l[[1]])){
    if(length(names(l)) < length(l)){
      names(l) <- paste('x', 1:length(l), sep = '.')
    }
    this$thisEnv$data_from_user <- l   
  }else if(xts::is.xts(l)){
    if(length(colnames(l)) < ncol(l)){
      colnames(l) <- paste('x', 1:ncol(l), sep = '.')
    }
    this$thisEnv$data_from_user <- l
  }else{
    stop('l must be a list or xts')
  }
  this$thisEnv$data_changed <- TRUE
}




#' Set your coefficients for any moment
#' 
#'
#' @param this modelStrategy
#' @param table xts, columns should go in same order, that in user data
#' @param force_fun logical, if it is TRUE, then beta_fun will be specified 
#' if it is FALSE, then it will be specified only if it is NULL
#'
#' @examples
#' \dontrun{
#' tmp <- xts(data.frame(GAZP = c(10, 20), LKOH = c(3, 4)), 
#'            c(as.Date('2010-01-01'), as.Date('2013-01-01')))
#' setBetaTable(this, tmp)
#' }
#' @export
#' @rdname setBetaTable
setBetaTable <- function(this, table, force_fun = FALSE){
  UseMethod('setBetaTable', this)
}

#' @export
#' @rdname setBetaTable
#' @method setBetaTable modelStrategy
setBetaTable.modelStrategy <- function(this, table, force_fun = FALSE){
  if(xts::is.xts(table)){
    this$thisEnv$user_beta_table <- table
    this$thisEnv$user_beta_table_changed <- TRUE
    this$thisEnv$beta_fun_force <- force_fun
  }else{
    stop('Table must be a xts object')
  }
}


#' Add user data
#' 
#' Here user can add his own data in xts format
#'
#' @param this modelStrategy
#' @param ... named arguments
#' @export
#' @rdname addData
addData <- function(this, ...){
  UseMethod('addData', this)
}

#' @export
#' @rdname addData
#' @method addData modelStrategy
addData.modelStrategy <- function(this, ...){
  dots <- list(...)
  if(length(dots) > 0){
    for(i in 1:length(dots)){
      x <- dots[i]
      if(xts::is.xts(x[[1]])){
        this$thisEnv$user_add_data[[names(x)]] <- x[[1]]
        this$thisEnv$data_changed <- TRUE
      }else{
        stop('x must be a xts')
      }
    }
  }
  
  
}

#' Clear data that has been specified before
#'
#' @param this modelStrategy
#' @export
#' @rdname clearData
clearData <- function(this){
  UseMethod('clearData', this)
}

#' @export
#' @rdname clearData
#' @method clearData modelStrategy
clearData.modelStrategy <- function(this){
  this$thisEnv$user_add_data <- list()
}


#' Submit strategy to contest
#'
#' @param this modelStrategy
#' @param contest character, name of contest
#' @param verbose logical
#' @param session ssh_session
#' @export
#' @rdname submit
submit <- function(this, contest, session, verbose=FALSE){
  UseMethod('submit', this)
}

#' @export
#' @rdname submit
#' @method submit modelStrategy
submit.modelStrategy <- function(this, contest, session, verbose=FALSE){
  competeInContest(this, contest, session, method='submit', verbose)
}


competeInContest <- function(this, contest, session, method, verbose){
  if(missing(session)){
    session <- .env[['session']]
  }
  this$thisEnv$user_args <- list(action = method, contest = contest)
  this[['settings']] <- .settings
  this[['version']] <- packageVersion('stratbuilder2pub')
  if(verbose){
    cat('Before sending rdata\n')
  }
  send_rdata(session, this)
  if(verbose){
    cat('After sending rdata\n')
  }
  get_results(this, session, reports=NULL, verbose)
}


#' Evaluate strategy in contest
#'
#' @param this modelStrategy
#' @param contest character, name of contest
#' @param verbose logical
#' @param session ssh_session
#' @export
#' @rdname evaluate
evaluate <- function(this, contest, session, verbose=FALSE){
  UseMethod('evaluate', this)
}

#' @export
#' @rdname evaluate
#' @method evaluate modelStrategy
evaluate.modelStrategy <- function(this, contest, session, verbose=FALSE){
  competeInContest(this, contest, session, method='evaluate', verbose)
}


#' Add dockerfile and create docker container on the server
#' 
#' Your dockerfile should be with installed python and pip
#'
#' @param path character, path to dockerfile
#' @param dockername character, name of docker container
#' @param verbose logical
#' @param session ssh session
#'
#' @return character, an answer from the server
#' @export
addDocker <- function(path, dockername, session, verbose=FALSE){
  if(missing(session)){
    session <- .env[['session']]
  }
  if(file.exists(path)){
    lines <- readLines(path) 
    temp <- file.path(tempdir(), 'Dockerfile')
    writeLines(c(paste0('###', dockername), lines, paste0('RUN pip install flask')), temp)
    capture.output(ssh::scp_upload(session, temp))
    return(get_results(NULL, session, reports=NULL, verbose))
  }else{
    stop('path argument should be a path to existing file')
  }
}



