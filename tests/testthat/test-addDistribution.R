context('distributions')

test_that("addDistribution fills model correctly for component.type='beta'", {
  model <- function(n, maType='EMA'){
    this <- modelStrategy() 
    setBeta(this, function(...) 2) 
    setLookback(this, 0) 
    setLookForward(this, 100) 
    setIgnorePosition(this, TRUE)
    addIndicator(this, args = list(name = maType, x = quote(spread), n = n), as = 'ema',
                 lookback = n) 
    addRule(this, as = 'short',  
            condition = spread > ema,
            type = 'enter', 
            side = -1,
            oco = 'short'
    )
    
    addRule(this, as = 'short_exit',
            condition = sum(unrealized_money_last) > getMoney(this) / 20,
            pathwise = TRUE,
            type = 'exit',
            oco = 'short'
    )
    
    setMoney(this, 100000)
    return(this)
  }
  
  # One strategy
  this <- model(100)
  
  f1 <- function(...) 1
  f2 <- function(...) 2
  
  paramset <- 'TEST'
  addDistribution(this,
                  paramset.label = paramset,
                  component.type = 'beta',
                  variable = list(x = f1),
                  label = 'fun'
                  )
  
  expect_that(this$thisEnv$paramsets$TEST$distributions$fun$variable, is_a('list'))
  expect_that(names(this$thisEnv$paramsets$TEST$distributions$fun$variable), is_equivalent_to('x'))
  expect_that(this$thisEnv$paramsets$TEST$distributions$fun$variable$x, is_equivalent_to('f1'))
  expect_that(this$thisEnv$paramsets$TEST$distributions$fun$env, is_a('environment'))
  expect_that(this$thisEnv$paramsets$TEST$distributions$fun$component.type, is_equivalent_to('beta'))
  expect_that(this$thisEnv$paramsets$TEST$distributions$fun$component.label, is_equivalent_to('x'))
  
  addDistribution(this,
                  paramset.label = paramset,
                  component.type = 'beta',
                  variable = list(x = c(f1)),
                  label = 'fun'
  )
  
  expect_that(this$thisEnv$paramsets$TEST$distributions$fun$variable, is_a('list'))
  expect_that(names(this$thisEnv$paramsets$TEST$distributions$fun$variable), is_equivalent_to('x'))
  expect_that(this$thisEnv$paramsets$TEST$distributions$fun$variable$x, is_equivalent_to('f1'))
  expect_that(this$thisEnv$paramsets$TEST$distributions$fun$env, is_a('environment'))
  expect_that(this$thisEnv$paramsets$TEST$distributions$fun$component.type, is_equivalent_to('beta'))
  expect_that(this$thisEnv$paramsets$TEST$distributions$fun$component.label, is_equivalent_to('x'))
  
  addDistribution(this,
                  paramset.label = paramset,
                  component.type = 'beta',
                  variable = list(x = c(f1, f2)),
                  label = 'fun'
  )
  
  expect_that(this$thisEnv$paramsets$TEST$distributions$fun$variable, is_a('list'))
  expect_that(names(this$thisEnv$paramsets$TEST$distributions$fun$variable), is_equivalent_to('x'))
  expect_equal(this$thisEnv$paramsets$TEST$distributions$fun$variable$x, c('f1', 'f2'))
  expect_that(this$thisEnv$paramsets$TEST$distributions$fun$env, is_a('environment'))
  expect_that(this$thisEnv$paramsets$TEST$distributions$fun$component.type, is_equivalent_to('beta'))
  expect_that(this$thisEnv$paramsets$TEST$distributions$fun$component.label, is_equivalent_to('x'))
})









