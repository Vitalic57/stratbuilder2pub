#' Set list of parameters
#' 
#' Here you can define parameters for rules, indicators, program part and managers of position
#'
#' @param type character, it can be equal to rules, indicators, pp or pm 
#' @param args list, define your params here
#' @param this modelStrategy
#' @export
#' @rdname setParams
setParams <- function(this,
                      type,
                      args){
  UseMethod('setParams', this)
}

#' @export
#' @examples
#' \dontrun{
#' setParams(this,
#'        type = 'rules',
#'        args = list(n = 5, a = 2)
#' )
#' }
#' @rdname setParams
#' @method setParams modelStrategy 
setParams.modelStrategy <- function(this,
                                    type,
                                    args){
  type <- switch (tolower(type),
                  rule = ,
                  rules = 'rules',
                  indicator =,
                  indicators = 'indicators',
                  pm = ,
                  pms = 'pms',
                  pp = ,
                  pps = ,
                  programparts =,
                  programpart =,
                  program = 'pps'
  )
  this$thisEnv$params[[type]] <- args
}


#' Get list of parameters according to type.
#'
#' @param type character, it can be equal to rules, indicators, pp or pm 
#' @param this modelStrategy
#' @export
#' @rdname getParams
getParams <- function(this,
                      type){
  UseMethod('getParams', this)
}

#' @export
#' @rdname getParams
#' @method getParams modelStrategy
getParams.modelStrategy <- function(this,
                                    type){
  type <- switch (tolower(type),
                  rule = ,
                  rules = 'rules',
                  indicator =,
                  indicators = 'indicators',
                  pm = ,
                  pms = 'pms',
                  pps =,
                  pp = ,
                  programparts =,
                  programpart =,
                  program = 'pps'
  )
  return(this$thisEnv$params[[type]])
}
