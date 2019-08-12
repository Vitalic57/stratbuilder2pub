library(stratbuilder2pub)
library(TTR)

fileName <- '/home/vitaly/Documents/stratbuilder2/data-raw/foo.py'

{
  this <- modelStrategy()
  addObject(this, myfile = readChar(fileName, file.info(fileName)$size))
  addIndicator(this, args = list(name = 'mymodule$fun', x = quote(spread), n = 100), as = 'ema',
               lookback = 101) 
  addProgramPart(this,
                 as = 'pp1',
                 evolution = list(
                   init = quote({ 
                     mymodule <- reticulate::py_run_string(myfile)
                   })
                 ))
}

