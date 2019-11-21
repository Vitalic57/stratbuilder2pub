# in our database there are several types of data
# data_raw -- close price multiplied by lot size, shifted for futures
# data_roll -- close price multiplied by lot size, no shift here
# data_margin -- for now it equals to data_raw, but later it will be responsible for margin
this$thisEnv$spreadData <- 'data_raw' # from what data spread will be built

this$thisEnv$betaData <- 'data_raw' # what data will be passed to beta builder function