getwd()
usethis::use_package('magrittr', type = 'Imports')

packageVersion('stratbuilder2pub')

lines <- readLines('DESCRIPTION') 
ind <- which(grepl('Version: ', lines))[1]
line <- lines[ind]
cur_version <- strsplit(line, ':')[[1]][2] 
print(cur_version)
lines[ind] <- paste0('Version: ', '0.1.13')
writeLines(lines, 'DESCRIPTION')

setwd('/home/vitaly/Documents/stratbuilder2pub')
devtools::test('.')
devtools::document()

setwd('..')
devtools::install('stratbuilder2pub')

 # download new version of package to users
setwd('/home/vitaly/Documents/stratbuilder2pub')
x <- devtools::build(binary = TRUE, args = c('--preclean'))
xx <- "/home/vitaly/Documents/stratbuilder2pub.tar.gz"
file.rename(x, "/home/vitaly/Documents/stratbuilder2pub.tar.gz")
tryCatch(ssh::ssh_disconnect(session), error=function(e){})
session <- ssh::ssh_connect('vshishkov@142.93.143.142')
ssh::scp_upload(session, xx, '/usr/local/lib/backtest/')

# download to server
ssh::scp_upload(session, '/home/vitaly/R/x86_64-pc-linux-gnu-library/3.4/stratbuilder2pub', 
                '/home/vshishkov/backtestit/packrat/lib/x86_64-pc-linux-gnu/3.4.2')



#download examples
session <- ssh::ssh_connect('vshishkov@142.93.143.142')
ssh::scp_upload(session, '/home/vitaly/Documents/stratbuilder2pub/data-raw/example10.R', '/usr/share/backtest/main')
ssh::scp_upload(session, '/home/vitaly/Documents/models/Sentiment/Data/sber_result_table_1.xlsx', '/usr/share/backtest/main')


session <- ssh_connect('test_backtest_user@142.93.143.142', keyfile = '/home/vitaly/Documents/ilia')
session <- ssh_connect('svetlana@142.93.143.142', keyfile = '/home/vitaly/Documents/models/Sentiment/sveta')


peek_examples(session)

download_examples(session)


stratbuilder2pub::update_package('test_backtest_user@142.93.143.142', '/home/vitaly/.ssh/ilia')

#devtools::install_local('/home/vitaly/Documents/aaa/stratbuilder2pub', force = TRUE)


#devtools::install_local('/home/vitaly/Documents/stratbuilder2pub (2)', force = TRUE)


