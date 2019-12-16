getwd()
usethis::use_package('magrittr', type = 'Imports')



packageVersion('stratbuilder2pub')

setwd('/home/vitaly/Documents/stratbuilder2pub')
lines <- readLines('DESCRIPTION') 
ind <- which(grepl('Version: ', lines))[1]
line <- lines[ind]
cur_version <- strsplit(line, ':')[[1]][2] 
print(cur_version)
lines[ind] <- paste0('Version: ', '1.4.2')
writeLines(lines, 'DESCRIPTION')

setwd('/home/vitaly/Documents/stratbuilder2pub')
setwd('/home/ruslan/stratbuilder2pub')
devtools::test('.')
devtools::document()
devtools::check('.', build_args = c('--no-build-vignettes'), args = '--no-build-vignettes')

devtools::install('.', build_vignettes = FALSE)

devtools::build_vignettes(install = FALSE)

# download new version of package to users
setwd('/home/vitaly/Documents/stratbuilder2pub')
x <- devtools::build(binary = TRUE, args = c('--preclean'))
xx <- "/home/vitaly/Documents/stratbuilder2pub.tar.gz"
file.rename(x, "/home/vitaly/Documents/stratbuilder2pub.tar.gz")
tryCatch(ssh::ssh_disconnect(session), error=function(e){})
session <- ssh::ssh_connect('vshishkov@142.93.143.142')
ssh::scp_upload(session, xx, '/usr/local/lib/backtest/')
ssh::ssh_disconnect(session)
# s <- paste0('scp -l 8192 ', xx,' vshishkov@142.93.143.142:/usr/local/lib/backtest/')
# system(s)

# download to server
session <- ssh::ssh_connect('vshishkov@142.93.143.142')
ssh::scp_upload(session, '/home/vitaly/R/x86_64-pc-linux-gnu-library/3.4/stratbuilder2pub',
                '/home/vshishkov/backtestit/packrat/lib/x86_64-pc-linux-gnu/3.4.2')
ssh::ssh_disconnect(session)
# s <- paste0('scp -r /home/vitaly/R/x86_64-pc-linux-gnu-library/3.4/stratbuilder2pub
#             vshishkov@142.93.143.142:/home/vshishkov/backtestit/packrat/lib/x86_64-pc-linux-gnu/3.4.2')
# system(s)


#download examples
session <- ssh::ssh_connect('vshishkov@142.93.143.142')
ssh::scp_upload(session, '/home/vitaly/Documents/stratbuilder2pub/data-raw/example13.R', '/usr/share/backtest/main')

ssh::scp_upload(session, '/home/vitaly/Documents/models/Sentiment/Data/sber_result_table_1.xlsx', '/usr/share/backtest/main')


session <- ssh::ssh_connect('test_backtest_user@142.93.143.142', keyfile = '/home/vitaly/Documents/ilia')
session <- ssh_connect('svetlana@142.93.143.142', keyfile = '/home/vitaly/Documents/models/Sentiment/sveta')



session <- ssh_connect('agurevich@142.93.143.142', keyfile = '/home/vitaly/Documents/id_rsa')



peek_examples(session)

download_examples(session)


#devtools::install_local('/home/vitaly/Documents/aaa/stratbuilder2pub', force = TRUE)


#devtools::install_local('/home/vitaly/Documents/stratbuilder2pub (2)', force = TRUE)

# y <- getDatasets()[['SPX']] %>% 
#   gsub('\\.', ' ', .) %>%
#   {
#     tmp <- grepl('^X[0-9]', .); 
#     .[tmp] <- gsub('X', '', .[tmp]); .
#   } %>% 
#   { 
#     tmp <- strsplit(., ' ') %>% sapply(length)== 4
#     .[tmp] <- .[tmp] %>% sub(' ', '-', .)
#     .
#   }
# 
# write.csv(data.frame(y), 'companies.csv')



create_contest <- function(contestName, endDate, method='public'){
  endDate <- endDate %>% as.Date %>% {paste0(., 'T23:59:59.999')}
  resp <- httr::POST(url = paste0("http://", address[[method]], "/contest"), httr::add_headers('Content-Type'='application/json;charset=UTF-8',
                                                                                               "Authorization"=token),
                     body = paste0('{"contestName":"', contestName,'","endDate":"', endDate,'"}')) 
  
  httr::content(resp)
}


create_contest('Mean reversion', '2019-05-31')





setwd('/stratbuilder2pub')
devtools::document()
setwd('..')
devtools::install('stratbuilder2pub')






x <- readRDS('/home/vitaly/Desktop/model.RData')

old <- readRDS('/home/vitaly/Desktop/orig.RData')

plotPnL(x)



