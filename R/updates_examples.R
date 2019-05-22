#' Update this package 
#'
#' @param user character, ssh address
#' @param key character, path to private key
#'
#' @export
update_package <- function(user, keyfile, session=NULL){
  f_inst <- function(x){
    list.of.packages <- x
    new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
    if(length(new.packages)) install.packages(new.packages)
  }
  f_inst(c('ssh', 'devtools'))
  if(is.null(session)){
    if(missing(user)){
      session <- .env[['session']]
    }else{
      if(missing(keyfile)){
        session <- ssh::ssh_connect(user)
      }else{
        session <- ssh::ssh_connect(user, keyfile = keyfile)
      }
    }
  }
  x <- capture.output(ssh::scp_download(session, '/usr/local/lib/backtest/stratbuilder2pub.tar.gz', tempdir()))
  tryCatch({
    detach("package:stratbuilder2pub", unload = TRUE)
  }, error = function(e){})
  if('stratbuilder2pub' %in% installed.packages()[,'Package']){
    suppressMessages(remove.packages("stratbuilder2pub"))
  }
  if(Sys.info()['sysname'] == 'Windows'){
    untar(file.path(tempdir(), 'stratbuilder2pub.tar.gz'), exdir = tempdir())
    dir <- getwd()
    setwd(tempdir())
    a <- shell('zip -r stratbuilder2pub.zip stratbuilder2pub', intern = TRUE)
    install.packages(file.path(tempdir(), 'stratbuilder2pub.zip'), repos = NULL, type = 'source')
    devtools::install_deps('stratbuilder2pub')
    shell('rm -r stratbuilder2pub')
    shell('rm stratbuilder2pub.tar.gz stratbuilder2pub.zip')
    setwd(dir)
  }else{
    install.packages(file.path(tempdir(), 'stratbuilder2pub.tar.gz'), repos = NULL, type = 'source')
    untar(file.path(tempdir(), 'stratbuilder2pub.tar.gz'), exdir = tempdir())
    devtools::install_deps(file.path(tempdir(), 'stratbuilder2pub'))
    system(paste0('rm -r ', file.path(tempdir(), 'stratbuilder2pub')))
    file.remove(file.path(tempdir(), 'stratbuilder2pub.tar.gz'))
    invisible()
  }
  hook <- paste0('setHook(packageEvent("stratbuilder2pub", "onLoad"), function(...) stratbuilder2pub::ssh_connect("', user, '", keyfile = "', keyfile, '"))')
  if(!file.exists('~/.Rprofile')){
    writeLines(hook, '~/.Rprofile')
  }else{
    lines <- readLines('~/.Rprofile') 
    ind <- which(grepl('stratbuilder2pub', lines) & grepl('onLoad', lines))[1]
    if(is.na(ind)){
      lines[length(lines) + 1] <- hook
    }else{
      lines[ind] <- hook
    }
    writeLines(lines, '~/.Rprofile')
  }
  library(stratbuilder2pub)
}


#' Upload example to server
#'
#' @param session ssh_session
#' @param file_path character, path to file
#' @param file_name character, how to name your file on server,
#' if NULL, then name will be the same as in your machine
#'
#' @export
upload_example <- function(session, file_path, file_name = NULL){
  name <- capture.output(ssh::ssh_exec_wait(session, 'realpath --relative-to="../" .'))[1]
  if(is.null(file_name)){
    capture.output(ssh::scp_upload(session, file_path, file.path('/usr/share/backtest/', name)))
  }else{
    capture.output(ssh::ssh_exec_wait(session, paste0('mv ', 
                                                      file.path('/usr/share/backtest', name, basename(file_path)), ' ',
                                                      file.path('/usr/share/backtest', name, file_name))
    ))
  }
  invisible(NULL)
}

#' Downloads directory of examples of some user
#'
#' @param session ssh_session
#' @param dir character, name of dir, by default it equals to main
#' @param to character, path where you want to store examples
#'
#' @export
download_examples <- function(session, dir = 'main', to = getwd()){
  if(missing(session)){
    session <- .env[['session']]
  }
  l <- head(capture.output(ssh::ssh_exec_wait(session, file.path('ls /usr/share/backtest/', dir))), -1) 
  suppressWarnings(dir.create(file.path(to, dir)))
  for(x in l){
    capture.output(ssh::scp_download(session, file.path('/usr/share/backtest/', dir, x), file.path(to, dir)))
  }
  invisible(NULL)
}

#' Peek examples
#' 
#' Peek how many examples each user have
#'
#' @param session ssh_session
#'
#' @return list
#' @export
peek_examples <- function(session){
  dirs <- head(capture.output(ssh::ssh_exec_wait(session, 'ls /usr/share/backtest/')), -1)
  res <- numeric(0)
  for(x in dirs){
    res[x] <- head(capture.output(ssh::ssh_exec_wait(session, paste0('ls /usr/share/backtest/', x, ' -1 | wc -l'))), -1)
  }
  return(res)
}