#' Do backtest of combinations from paramset
#'
#' @param this modelStrategy
#' @param paramset.label charater, name of paramset
#' @param nsamples numeric, how many backtests to do
#' @param start_date Date / charater in format 'yyyy-mm-dd', start date of simulation
#' @param end_date Date / charater in format 'yyyy-mm-dd', end date of simulation
#' @param session ssh_session
#'
#' @return data.frame, reports of simulations
#' @export
applyParamsetServer.modelStrategy <- function(this, 
                                              session,
                                              paramset.label, 
                                              nsamples = 100,
                                              verbose=FALSE, 
                                              ...){
  this$thisEnv$user_args <- c(list(action = 'applyParamset',
                                 nsamples = nsamples, 
                                 paramset.label = paramset.label),
                              list(...))
  this[['settings']] <- stratbuilder2pub:::.settings
  # upload part  ---------------------------
  this[['version']] <- packageVersion('stratbuilder2pub')
  send_rdata(session, this)
  if(!('reload' %in% names(stratbuilder2pub:::.settings) && stratbuilder2pub:::.settings[['reload']])){
    this$thisEnv$data_changed <- FALSE
    this$thisEnv$user_beta_table_changed <- FALSE
  }
  Sys.sleep(3)
  get_results(session, 'strategy', verbose)[[1]]
}





#' Apply paramset to list of strategies 
#'
#' @param l, list of modelStrategy
#' @param paramset.label charater, name of paramset
#' @param nsamples numeric, how many backtests to do
#' @param start_date Date / charater in format 'yyyy-mm-dd', start date of simulation
#' @param end_date Date / charater in format 'yyyy-mm-dd', end date of simulation
#' @param session ssh_session
#'
#' @return list, list of data.frames (reports of simulations)
#' @export
applyParamsetServer.list <- function(l, 
                                    session,
                                    paramset.label, 
                                    nsamples = 100,
                                    verbose=FALSE,
                                    ...){
  l <- set_names_list(l)
  e <- new.env()
  e[['strategies']] <- l
  e[['user_args']] <- c(list(action = 'applyParamset',
                           nsamples = nsamples, 
                           paramset.label = paramset.label),
                        list(...))
  e[['settings']] <- stratbuilder2pub:::.settings
  e[['data_changed']] <- TRUE#any(sapply(l, function(x) x$thisEnv$data_changed))
  e[['version']] <- packageVersion('stratbuilder2pub')
  send_rdata(session, e)
  # for(this in l){
  #   this$thisEnv$data_changed <- FALSE
  #   this$thisEnv$user_beta_table_changed <- FALSE
  # }
  
  Sys.sleep(3)
  return(get_results(session, 'strategy', verbose)[[1]])
}








#' Send your strategy to server for backtesting.
#' 
#' All results could be seen in your folder on server.
#'
#' @param this modelStrategy
#' @param session ssh_session
#' @param reports character vector, if 'strategy' exists inside this vector, then report of whole period will be given
#' if 'calendar' specified, then report about each year will be given
#' if 'trades', then report about trades will be sent
#' @param ... additional arguments:
#' start_date -- from this date backtest will be started
#' end_date -- at this date backtest will be ended. Max end_date is Sys.Date() - 365
#' paramset.label -- character, name of paramset
#' paramset.index -- numeric, index of combination of params to backtest, by defualt current strategy will be backtested
#' 
#' @return data.frame, report of strategy
#' @export
performServer.modelStrategy <- function(this, 
                                        session,
                                        reports = c('strategy', 'calendar', 'trades', 'plot'),
                                        verbose=FALSE,
                                        ...){
  this$thisEnv$user_args <- c(list(...), list(action = 'perform', reports = reports))
  this[['settings']] <- stratbuilder2pub:::.settings
  this[['version']] <- packageVersion('stratbuilder2pub')
  if(verbose){
    cat('Before sending data\n')
  }
  send_rdata(session, this)
  if(verbose){
    cat('After sending data\n')
  }
  if(!('reload' %in% names(stratbuilder2pub:::.settings) && stratbuilder2pub:::.settings[['reload']])){
    this$thisEnv$data_changed <- FALSE
    this$thisEnv$user_beta_table_changed <- FALSE
  }
  Sys.sleep(0.5)
  if(verbose){
    cat('Before getting results\n')
  }
  get_results(session, reports, verbose)

}

#' Simulate list of strategies
#'
#' @param l list, list of strategies
#' @param session ssh_session
#' @param reports character vector, it can include c('strategy', 'calendar', 'trades', 'plot')
#' @param ... additional params
#'
#' @return list
#' @export
performServer.list <- function(l, session, reports =  c('strategy', 'calendar', 'trades', 'plot'), verbose=FALSE, ...){
  l <- set_names_list(l)
  e <- new.env()
  e[['strategies']] <- l
  e[['user_args']] <- c(list(...), list(action = 'perform', reports = reports))
  # e[['user_args']] <- c(list(action = 'perform', reports = reports))
  e[['settings']] <- stratbuilder2pub:::.settings
  e[['data_changed']] <- TRUE#any(sapply(l, function(x) x$thisEnv$data_changed)) 
  e[['version']] <- packageVersion('stratbuilder2pub')
  send_rdata(session, e, verbose)
  # for(this in l){
  #   this$thisEnv$data_changed <- FALSE
  #   this$thisEnv$user_beta_table_changed <- FALSE
  # }
  Sys.sleep(0.5)
  get_results(session, reports, verbose)
}

send_rdata <- function(session, obj, verbose=FALSE){
  file_path <- file.path(tempdir(), 'file.RData')
  saveRDS(obj, file_path)
  tryCatch({
    capture.output(ssh::scp_upload(session, file_path))
  }, error = function(e){})
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

#' @export
ssh_info <- function(session){
  info <- ssh::ssh_info(session)
  info$keyfile <- .env[['keyfile']]
  return(info)
}


.env <- new.env()

#' @export
ssh_connect <- function(host, keyfile = NULL, passwd = ssh:::askpass, verbose = FALSE){
  .env[['keyfile']] <- keyfile
  session <- ssh::ssh_connect(host=host, keyfile = keyfile, passwd = passwd, verbose = verbose)
  return(session)
}



get_results <- function(session, reports, verbose=FALSE){

  # wait for results--------------------------
  files_path <- file.path(tempdir())
  
  vec_cond <- logical(0)
  vec_names <- c(pnl = 'pnl.png', strategy = 'report.RDS', calendar = 'report2.RDS', trades = 'trades.RDS')
  vec_cond['pnl'] <- 'plot' %in% reports
  vec_cond['strategy'] <-  'strategy' %in% reports
  vec_cond['calendar'] <-  'calendar' %in% reports
  vec_cond['trades'] <-  'trades' %in% reports
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
    cat('Results got\n')
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
      scp_download(session, paste0('last_results/', vec_names[x]), files_path, verbose = TRUE)
      if(verbose){
        print('downloaded')
      }
    }
  })
  if(verbose){
    cat('Files of results downloaded\n')
  }
  
  # show results ----------------------------------------
  results <- list()
  x <- sapply(2:4, function(i){
    x <- names(vec_names)[i]
    if(vec_cond[x] && vec_avail[x]){
      txt_path <- file.path(files_path, vec_names[x])
      results[[length(results) + 1]] <<- readRDS(txt_path)
      file.remove(txt_path)
    }
  })
  if(verbose){
    cat('Reports downloaded\n')
  }
  
  if(vec_cond['pnl'] && vec_avail['pnl']){
    png_path <- file.path(files_path, 'pnl.png')
    # image <- imager::load.image(png_path)
    # imager:::plot.cimg(image, axes = FALSE)
    img <- png::readPNG(png_path)
    grid::grid.raster(img)
    file.remove(png_path)
    Sys.sleep(1)
    if(verbose){
      cat('Image downloaded')
    }
  }
  
  return(results)
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
#' setUserData(this, list(
#'    dataset = 'Russia',
#'    period = 'day',
#'    assets = c('GAZP', 'LKOH'),
#'    time = 19
#' ))
#' 
#' x <- xts(cumsum(rnorm(1000)) + 300, Sys.Date()  + 1:1000)
#' setUserData(this, x)
setUserData.modelStrategy <- function(this, l){
  if(is.list(l) && !xts::is.xts(l[[1]])){
    if(!all(c('dataset', 'period', 'assets') %in% names(l))){
      stop('l must contain dataset, period and assets fields')
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
    if(!all(l[['assets']] %in% datasets[[l[['dataset']]]])){
      stop('Names of assets must be from your selected dataset. Please check names in stratbuilder2pub:::datasets.')
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
#' @export
#'
#' @examples
#' tmp <- xts(data.frame(GAZP = c(10, 20), LKOH = c(3, 4)), c(as.Date('2010-01-01'), as.Date('2013-01-01')))
#' setBetaTable(this, tmp)
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
#' @param x xts
#'
#' @export
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
#'
#' @export
clearData.modelStrategy <- function(this){
  this$thisEnv$user_add_data <- list()
}



