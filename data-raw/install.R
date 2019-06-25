# ENTER YOUR PARAMETERS !!!!!!!!!!!!!!!!!!!
user <- 'YOUR ADDRESS'
key <- 'YOUR PATH TO KEY'
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

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
  if(missing(user)){
    user <- paste0(ssh::ssh_info(.env$session)[['user']], '@', ssh::ssh_info(.env$session)[['host']])
  }
  if(missing(keyfile)){
    if('keyfile' %in% names(.env)){
      keyfile <- .env$keyfile
    }else{
      stop('Enter keyfile')
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
update_package(user, key)

download_examples()

# now examples in
print(file.path(getwd(), 'main'))











