library(stratbuilder2pub)
library(TTR)
library(readxl)
library(magrittr)
library(xts)

file_name <- 'data_07_12_2018.xlsx'
sheets <- readxl::excel_sheets(file_name) %>% tail(-3)
data <- lapply(sheets, function(x){
  y <- read_excel(file_name, sheet = x, na = '#N/A N/A', col_types = c('date', rep('numeric', 5))) %>%
    {xts(.[,-1], dplyr::pull(.[,1]))} %>%
    na.omit %>%
    set_colnames(c('Open', 'Close', 'Volume', 'Low', 'High'))
  if(nrow(y) > 252 * 10){
    y
  }else{
    NULL
  }
}) %>%
  set_names(sheets)%>%
  {
    .[sapply(., is.null)] <- NULL
    .
  }

length(data)

# Example of usage for multiple asset and rebalancing of portfolio
{
  this <- modelStrategy() 
  setLookback(this, 100) 
  setLookForward(this, 30) 
  setIgnorePosition(this, TRUE) 
  setBeta(this, function(data, ...){ 
    data <- as.numeric(tail(data, 1) )
    ord <- order(data)
    beta <- numeric(length(data))
    beta[tail(ord, 5)] <- 1
    #print(beta)
    return(beta)
  }) 
  addRule(this, as = 'long', 
          condition = TRUE,
          type = 'enter',
          side = 1,
          oco = 'long'
  )
  addProgramPart(this, as = 'sdf',
                 evolution = list(
                   data = quote({
                     modelD[['RSI']] <<- modelD[['candles']] %>% 
                       lapply(function(x){
                         RSI(Cl(x), 20)
                       }) %>%
                       Reduce('cbind', .) %>%
                       coredata
                   })
                 ))
  this$thisEnv$betaData <- 'RSI'
}

setUserData(this, data) 


performServer(this)







