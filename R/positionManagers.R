

#' @export
#' @rdname getPM
getPM <- function(this, recalc, ...){
    UseMethod('getPM', this)
}




#Position managers
#' Gets all position managers
#'
#' @param this modelStrategy
#' @param recalc logical
#' @param ... params
#'
#' @return list
#' @export
#' @rdname getPM
#' @method getPM modelStrategy
getPM.modelStrategy <- function(this, recalc = FALSE, ...){
    if(recalc){
        calls_market <- rlang::enexprs(...)
        if('in_out' %in% names(calls_market)){
            calls_market[['in_out']] <- eval(calls_market[['in_out']], envir = parent.frame())
        }
        pms.params <- getParams(this, 'pms')
        for(i in seq_along(this$thisEnv$positionManagers)){
            pm <- this$thisEnv$positionManagers[[i]]
            my_name <- pm$as
            if('rule_name' %in% names(pm)){
                my_name <- pm$rule_name
            }
            last_if <- call('{')
            
            # DECREASE
            if(!pm$decrease[['cond']] == FALSE){
                dec_quote <- call('{',
                                  quote(mult_delta <- e_user[['mult_delta']]),
                                  quote(mult_delta <- floor(abs(mult_delta))),
                                  call('if', quote(abs(mult_delta) > 0), call('{',
                                                                              call('if', quote(abs(mult_delta) >= mult_last), call('{',
                                                                                                                                   quote(rule_exit <- paste('pm.deacrease', x, sep = '.')),
                                                                                                                                   calls_market$in_out
                                                                              ), call("{",rlang::expr({
                                                                                  mult_last <- mult_last - mult_delta
                                                                                  position_changed <- TRUE
                                                                                  money_in_pos_leg_last <- abs(mult_last) * data_margin[i,] * abs(beta_last)
                                                                                  active_rule <- paste(!!my_name,"decrease", sep = "_")
                                                                                  change_active <- -mult_delta*beta_last
                                                                                  amount_active <- mult_last*beta_last
                                                                                  money_in_pos_last <- sum(money_in_pos_leg_last)
                                                                              }),
                                                                              perform_calls[['add_to_trading_log']]
                                                                              )))
                                  ))
                dec_quote <- pryr::substitute_q(dec_quote, env = list(x = pm$as))
                dec_expr <- call('{',
                                 call('with', quote(e_user), pm$decrease[['expr']]),
                                 dec_quote
                )
                
                if(length(getTradeTime(this, 'decrease')) > 0){
                    last_if <- call('if', call('with', quote(e_user), call('&&',
                                                                           pm$decrease[['cond']],
                                                                           quote(time_events[['decrease']][i])
                    )), dec_expr)
                }else{
                    last_if <- call('if', call('with', quote(e_user), pm$decrease[['cond']]), dec_expr)
                }
            }
            
            #INCREASE
            if(!pm$increase[['cond']] == FALSE){
                inc_quote <- call("{",rlang::expr({
                    mult_delta <- floor(abs(e_user[['mult_delta']]))
                    if(abs(mult_delta) > 0){
                        position_changed <- TRUE
                        mult_last <- mult_last + mult_delta
                        money_in_pos_leg_last <- abs(mult_last) * data_margin[i,] * abs(beta_last)
                        money_in_pos_last <- sum(money_in_pos_leg_last)
                        active_rule <- paste(!!my_name,"increase", sep = "_")
                        change_active <- -mult_delta*beta_last
                        amount_active <- mult_last*beta_last
                    }}), perform_calls[['add_to_trading_log']]
                )
                inc_expr <- call('{',
                                 call('with', quote(e_user), pm$increase[['expr']]),
                                 inc_quote
                )
                
                if(length(getTradeTime(this, 'increase')) > 0){
                    last_if <- call('if', call('with', quote(e_user), call('&&',
                                                                           pm$increase[['cond']],
                                                                           quote(time_events[['increase']][i])
                    )), inc_expr, last_if)
                }else{
                    last_if <- call('if', call('with', quote(e_user), pm$increase[['cond']]), inc_expr, last_if)
                }
            }
            
            #CHANGE
            if(!pm$change[['cond']] == FALSE){
                chg_quote <- call('{',
                                  quote(mult_delta <- e_user[['mult_delta']]),
                                  quote(mult_delta <- floor(abs(mult_delta)) * sign(mult_delta)),
                                  call('if', quote(abs(mult_delta) > 0), call('{',
                                                                              call('if', quote(abs(mult_delta) >= mult_last && sign(mult_delta) < 0), call('{',
                                                                                                                                                           quote(rule_exit <- paste('pm.change', x, sep = '.')),
                                                                                                                                                           calls_market$in_out
                                                                              ), call("{", rlang::expr({
                                                                                  mult_delta <- abs(mult_delta) * sign(mult_delta)
                                                                                  mult_last <- mult_last + mult_delta
                                                                                  position_changed <- TRUE
                                                                                  money_in_pos_leg_last <- abs(mult_last) * data_margin[i,] * abs(beta_last)
                                                                                  money_in_pos_last <- sum(money_in_pos_leg_last)
                                                                                  active_rule <- paste(!!my_name,"change", sep = "_")
                                                                                  change_active <- -mult_delta*beta_last
                                                                                  amount_active <- mult_last*beta_last
                                                                                  money_in_pos_last <- sum(money_in_pos_leg_last)
                                                                              }),
                                                                              perform_calls[['add_to_trading_log']]
                                                                              )
                                                                              )
                                  ))
                )
                chg_quote <- pryr::substitute_q(chg_quote, env = list(x = pm$as))
                chg_expr <- call('{',
                                 call('with', quote(e_user), pm$change[['expr']]),
                                 chg_quote
                )
                
                if(length(getTradeTime(this, 'change')) > 0){
                    last_if <- call('if', call('with', quote(e_user), call('&&',
                                                                           pm$change[['cond']],
                                                                           quote(time_events[['change']][i])
                    )), chg_expr, last_if)
                }else{
                    last_if <- call('if', call('with', quote(e_user), pm$change[['cond']]), chg_expr, last_if)
                }
            }
            
            #REBALANCE
            if('rebalance' %in% names(pm)){
                if(!pm$rebalance[['cond']] == FALSE){
                    reb_expr <- call('{')
                    if('money' %in% names(pm$rebalance)){
                        reb_expr[[length(reb_expr) + 1]] <- call('<-', quote(pm_money), call('with', quote(e_user), pm$rebalance[['money']]))
                    }else{
                        reb_expr[[length(reb_expr) + 1]] <- call('<-', quote(pm_money), quote(money_in_pos_last))
                    }
                    if('betas' %in% names(pm$rebalance)){
                        reb_expr[[length(reb_expr) + 1]] <- call('<-', quote(beta_new), call('with', quote(e_user), pm$rebalance[['betas']]))
                    }else{
                        reb_expr[[length(reb_expr) + 1]] <- call('<-', quote(beta_new), quote(beta_last))
                    }
                    reb_expr_out <- reb_expr
                    reb_expr <- call('{')
                    if('betas' %in% names(pm$rebalance)){
                        if('by_money' %in% names(pm$rebalance) && pm$rebalance[['by_money']]){
                            reb_expr[[length(reb_expr) + 1]] <-
                                quote(beta_new <- pm_money * abs(beta_new) / sum(abs(beta_new)) / data_margin[i,] * sign(beta_new))
                        }
                    }else if('expr' %in% names(pm$rebalance)){
                        reb_expr[[length(reb_expr) + 1]] <- call('with', quote(e_user), pm$rebalance[['expr']])
                        reb_expr[[length(reb_expr) + 1]] <- quote(beta_new <- e_user[['beta_new']])
                        if('by_money' %in% names(pm$rebalance) && pm$rebalance[['by_money']]){
                            reb_expr[[length(reb_expr) + 1]] <-
                                quote(beta_new <- pm_money * abs(beta_new) / sum(abs(beta_new)) / data_margin[i,] * sign(beta_new))
                        }
                    }
                    # general actions
                    reb_expr[[length(reb_expr) + 1]] <- rlang::expr({
                        if(any(beta_new %% 1 > 0.00001)){
                            beta_new <- Beta_round(beta_new, y = getToleranceBeta(this))
                        }
                        beta_new <- floor(abs(beta_new)) * sign(beta_new)
                        pos_start_iter <- mult_last*beta_last*side_last
                        mult_last <- sameMoneyOs(beta=beta_new, amount=pm_money, price=data_margin[i,])
                        money_in_pos_leg_last <- abs(mult_last) * data_margin[i,] * abs(beta_new)
                        money_in_pos_last <- sum(money_in_pos_leg_last)
                        beta_last <- beta_new
                        position_changed <- TRUE
                        active_rule <- paste(!!my_name,"rebalance", sep = "_")
                        amount_active <- mult_last*beta_last*side_last
                        change_active <- -pos_start_iter + amount_active
                        money_in_pos_last <- sum(money_in_pos_leg_last)
                        
                    })
                    reb_expr[[length(reb_expr) + 1]] <- perform_calls[['add_to_trading_log']]
                    
                    # if something is NULL then omit further actions
                    reb_expr_out[[length(reb_expr_out) + 1]] <- call('if', quote(!is.null(pm_money) && !is.null(beta_new)), reb_expr)
                    reb_expr <- reb_expr_out
                    
                    if(length(getTradeTime(this, 'rebalance')) > 0){
                        last_if <- call('if', call('with', quote(e_user), call('&&',
                                                                               pm$rebalance[['cond']],
                                                                               quote(time_events[['rebalance']][i])
                        )), reb_expr, last_if)
                    }else{
                        last_if <- call('if', call('with', quote(e_user), pm$rebalance[['cond']]), reb_expr, last_if)
                    }
                }
            }
            
            #CLOSE
            if(!pm$close[['cond']] == FALSE){
                q <- rlang::expr(rule_exit <- paste('pm.close', !!my_name, sep = '.'))
                close_expr <- call('{',
                                   q,
                                   calls_market$in_out
                )
                if(length(getTradeTime(this, 'exit')) > 0){
                    last_if <- call('if', call('&&',
                                               call('with', quote(e_user), pm$close[['cond']]),
                                               quote(time_events[['exit']][i])
                    ), close_expr, last_if)
                }else{
                    last_if <- call('if', call('with', quote(e_user), pm$close[['cond']]), close_expr, last_if)
                }
            }
            
            
            if(!"rule_name" %in% names(pm)){
                pm$call <- pryr::substitute_q(last_if, c(pm$args, pms.params))
            }else{
                pm$call <- pryr::substitute_q(last_if, c(getRule(this, pm$rule_name)[['args']], getParams(this, 'rules')))
            }
            pm$call <-  pryr::substitute_q(pm$call, env = list(x = pm$as))
            
            this$thisEnv$positionManagers[[i]] <- pm
        }
    }
    return(this$thisEnv$positionManagers)
}




#' @export
#' @rdname addPM
addPM <- function(this, ...){
    UseMethod('addPM', this)
}



#' Creator for position manager.
#'
#' @param this modelStrategy
#' @param as character, name
#' @param increase list, the first argument is resposible for condition when this rule will work,
#' the second argument is expression that must calculate size of position(mult_delta)(how many spreads it needs to be bought).
#' In another words the last row must contain 'mult_delta <- ...'. Or you could define 'money' variable. It should be equal to
#' current amount of money in position after execution of the rule.
#' @param decrease list, see increase, in expr quote beta_new must be defined
#' @param close list, see increase
#' @param oco charcter, name of namespace
#' @param rebalance list, see increase, in expr quote beta_new must be defined
#' @param change list, see increase, in expr quote mult_delta must be defined
#' @param args list, arguments of that manager
#' @param ... params
#' @param rule_name character, if it is determined, then this manager will be addicted to rule by this name and
#' params of this rule will be applied.
#'
#' @export
#' @rdname addPM
#' @method addPM modelStrategy
addPM.modelStrategy <- function(this,
                                as,
                                increase = list(cond = quote(FALSE), expr = quote({})),
                                decrease = list(cond = quote(FALSE), expr = quote({})),
                                rebalance = list(cond = quote(FALSE), expr = quote({})),
                                change = list(cond = quote(FALSE), expr = quote({})),
                                close = list(cond = quote(FALSE)),
                                oco,
                                args = list(),
                                rule_name = NULL,
                                ...
){
    e <- this$thisEnv
    if(missing(as)){
        as <- paste0('x', length(e$positionManagers) + 1)
    }
    as <- gsub('\\.','_',as)
    if(missing(oco)){
        oco <- as
    }
    e$positionManagers[[as]] <- list(
        as = as,
        increase = quote_list(rlang::enexpr(increase), parent.frame()),
        decrease = quote_list(rlang::enexpr(decrease), parent.frame()),
        rebalance = quote_list(rlang::enexpr(rebalance), parent.frame()),
        change = quote_list(rlang::enexpr(change), parent.frame()),
        close = quote_list(rlang::enexpr(close), parent.frame()),
        oco = oco,
        args = args
    )
    if(!is.null(rule_name)){
        e$positionManagers[[as]][['rule_name']] <- rule_name
    }
}







