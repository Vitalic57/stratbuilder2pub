f_inst <- function(x){
  list.of.packages <- x
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages, repos = "http://cran.us.r-project.org")
}
f_inst(c('ssh', 'devtools'))
devtools::document('/stratbuilder2pub')
devtools::install('stratbuilder2pub')
devtools::install_deps('/stratbuilder2pub')