fun <- function(a, b){
  for(name in names(a)){
    if(name %in% c('thisEnv', 'modelD', 'me')){
      next
    }
    if(name %in% names(b)){
      if(is.list(a[[name]]) || is.environment(a[[name]])){
        #print(name)
        fun(a[[name]], b[[name]])
      }else if(is.function(a[[name]]) || is.xts(a[[name]]) || is.matrix(a[[name]]) || is.data.frame(a[[name]]) || length(a[[name]]) > 1){
        next
      }else{
        tryCatch({
          if(any(a[[name]] != b[[name]])){
            cat(paste0("values do not equal by name ", name, '\n'))
            cat(paste0(name, ": ", a[[name]], '\n'))
            cat(paste0(name, ": ", b[[name]], '\n'))
          }
        }, error = function(e){
          print(a[[name]])
        })
        
      }
    }else{
      print(paste0("no such name ", name, " in b"))
    }
  }
}
