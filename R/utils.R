set_names_list <- function(l){
  x <- names(l)
  if(is.null(x)){
    names(l) <- paste0('m_', seq_len(length(l)))
  }else{
    ind <- x == ''
    new_names <- x
    new_names[ind] <- paste0('m_', seq_len(length(l)))[ind]
    names(l) <- new_names
  }
  return(l)
}
