# .onLoad <- function(libname, pkgname, ...){
#   R.utils::setOption(x = "stratbuilder2pub", value = list(user = 'test_backtest_user@142.93.143.142',
#                                                           keyfile = '/home/vitaly/Documents/ilia'))
#   
#   options(stratbuilder2pub = list(user = 'test_backtest_user@142.93.143.142',
#                                   keyfile = '/home/vitaly/Documents/ilia'))
#   
#   op <- options()[['stratbuilder2pub']]
#   if(!is.null(op) && 'user' %in% names(op) && keyfile %in% names(op)){
#     ssh_connect(op[['user']], keyfile = op[['keyfile']])
#   }
# }
