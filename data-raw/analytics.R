dep_profit_stop <- function(report, dist='unif', target='return'){
  report <- report[,c('MAE.with.com', 'pnl.sum.adj')]
  mae <- report[,c('MAE.with.com')]
  ret <- report[,c('pnl.sum.adj')]
  max_mae <- max(mae)
  min_mae_neg <- min(mae[ret < 0])
  stop_losses <- exp(seq(log(min_mae_neg), log(max_mae * .99), length.out = 100))
  fun <- switch (dist,
                 unif = {
                   function(x, stop_loss){
                     runif(1, stop_loss, x)
                   }
                 },
                 beta = {
                   function(x, stop_loss){
                     rbeta(1, 2, 6) * (x - stop_loss) + stop_loss
                   }
                 },
                 stop_loss=,
                 stop = {
                   function(x, stop_loss){
                     stop_loss
                   }
                 }
  )
  res <- sapply(stop_losses, function(stop_loss){
    c(ret[mae < stop_loss], -sapply(mae[mae >= stop_loss], fun, stop_loss=stop_loss))
  })
  if(target == 'return'){
    pnl_init <- sum(ret)
    #plot(stop_losses, apply(res, 2, sum) - pnl_init, type='l', ylab='base minus stop')
    dygraph(list(x = stop_losses, y = apply(res, 2, sum) - pnl_init))
  }else if(target == 'sharpe'){
    sharpe_init <- mean(ret) / var(ret) ^ 0.5
    #plot(stop_losses, apply(res, 2, function(x) mean(x) / var(x) ^ 0.5) - sharpe_init, type='l', ylab='base minus stop')
    dygraph(list(x = stop_losses, y = apply(res, 2, function(x) mean(x) / var(x) ^ 0.5) - sharpe_init))
  }else if(target == 'sortino'){
    sharpe_init <- mean(ret) / var(ret[ret < 0]) ^ 0.5
    #plot(stop_losses, apply(res, 2, function(x) mean(x) / var(x[x < 0]) ^ 0.5) - sharpe_init, type='l', ylab='base minus stop')
    dygraph(list(x = stop_losses, y = apply(res, 2, function(x) mean(x) / var(x[x < 0]) ^ 0.5) - sharpe_init))
  }
}
