# leaderboard api

token <- "Bearer GiOIHgMBLQvFi1XYPcX3Uow5F3oMRs23h1zUqK8wPlWFtp5BllIBgu9+8zY1eCUpaVZXifEmalC/ONLElvL5UQ"

get_all_users <- function(){
  resp <- httr::GET(url = "http://leaderboard.pfladvisors.com/get/user", httr::add_headers('Content-Type'='application/json;charset=UTF-8',
                                                                              "Authorization"=token))
  httr::content(resp) %>% {set_names(., lapply(., '[[', "userName"))}
}

get_all_contests <- function(){
  resp <- httr::GET(url = "http://leaderboard.pfladvisors.com/get/contest", httr::add_headers('Content-Type'='application/json;charset=UTF-8'))
  httr::content(resp) %>% {set_names(., lapply(., '[[', "contestName"))}
}

get_contest <- function(contestName, endDate){
  endDate <- endDate %>% as.Date %>% {paste0(., 'T23:59:59.999')}
  resp <- httr::POST(url = "http://leaderboard.pfladvisors.com/get/contest", httr::add_headers('Content-Type'='application/json;charset=UTF-8',
                                                                                         "Authorization"=token),
                     body = list(contentName = contestName, endDate = endDate))
  httr::content(resp)
}

create_user <- function(userName){
  resp <- httr::POST(url = "http://leaderboard.pfladvisors.com/get/user", httr::add_headers('Content-Type'='application/json;charset=UTF-8',
                                                                                               "Authorization"=token),
                     body = list(userName=userName, publicName=get_hash(userName)))
  #httr::content(resp)
}

create_score <- function(user, contest, score){
  resp <- httr::POST(url = "http://leaderboard.pfladvisors.com/get/user", httr::add_headers('Content-Type'='application/json;charset=UTF-8',
                                                                                            "Authorization"=token),
                     body = list(userId=get_all_users()[[user]][['id']], 
                                 contestId=get_all_contests()[[contest]][['id']], 
                                 score = score,
                                 lastSubmissionDate = Sys.time() %>% as.character %>% strsplit(' ') %>% .[[1]] %>% paste(collapse = 'T')
                                 ))
  #httr::content(resp)
}

get_hash <- function(x){
  digest::digest(x, seed = 57, algo = 'murmur32')
}



