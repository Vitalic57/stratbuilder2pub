
#' Plot graph maximum/minimum pnl in trade vs realized pnl
#'
#' @param report data.frame, must contain 3 columns 'MAE.with.com', 'MFE.with.com', 'pnl.sum.adj'. 
#' It can be report of trades, returned from simulation
#' @param type character, 'MFE' or 'MAE'
#'
#' @return ggplot
#' @export
plotReturns <- function(report, type = 'MAE'){
  switch(type,
         MAE=,
         min=,
         loss=,
         maxloss={
           var <- 'MAE.with.com'
         },
         MFE=,
         max=,
         profit=,
         maxprofit={
           var <- 'MFE.with.com'
         }
  )
  rets <- 'pnl.sum.adj'
  df <- report[,c(rets,var)] 
  colnames(df) <- c('rets','var')
  p <- ggplot(df, aes(abs(rets),var, group = rets < 0 )) +
    geom_point(aes(col = rets < 0, shape = rets  < 0) , size = 3) +
    #geom_point(aes(colour = rets > 0)) +
    #scale_shape(solid = FALSE) +
    scale_shape_manual(values=c(24,25)) +
    scale_color_manual(values=c('green','red')) +
    geom_abline(intercept = 1, linetype = 'dotted') + 
    theme_bw() +
    theme(legend.position="none") +
    labs(y = paste(strsplit(var,'\\.')[[1]][1]),
         x = paste0(strsplit(rets,'\\.')[[1]][1],'s')) +
    ggtitle(paste(strsplit(var,'\\.')[[1]][1] ,'vs', paste0(strsplit(rets,'\\.')[[1]][1],'s') ) )
  return(p)
}


