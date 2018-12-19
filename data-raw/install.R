user <- 'YOUR ADDRESS'
key <- 'YOUR PATH TO KEY'
update_package <- function(user, keyfile, session=NULL){
  f_inst <- function(x){
    list.of.packages <- x
    new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
    if(length(new.packages)) install.packages(new.packages)
  }
  f_inst(c('ssh', 'devtools'))
  if(is.null(session)){
    if(missing(keyfile)){
      session <- ssh::ssh_connect(user)
    }else{
      session <- ssh::ssh_connect(user, keyfile = keyfile)
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
  library(stratbuilder2pub)
}
update_package(user, key)

library(stratbuilder2pub)


session <- ssh::ssh_connect(user, keyfile = key)
download_examples(session)

# now examples in
print(file.path(getwd(), 'main'))
