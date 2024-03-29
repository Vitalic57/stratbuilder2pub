% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/programParts.R
\name{addProgramPart}
\alias{addProgramPart}
\alias{addProgramPart.modelStrategy}
\title{Adds variables to strategy}
\usage{
addProgramPart(this, as, evolution, ...)

\method{addProgramPart}{modelStrategy}(this, as, evolution = list(), ...)
}
\arguments{
\item{this}{modelStrategy}

\item{as}{character, name}

\item{evolution}{list, name of each element is the place where code will be executed, and value is quoted expression}

\item{...}{callbacks
There are multiple places where you can add your code :
i. full name / short name -- description. 

1. init -- in initialization period before main cycle

2. after_tables / tables -- after initialization of rules tables

3. after_coefs / coefs -- after initialization of coeffitients

4. each_iter / iter -- in the beginnig of each iteration, before execution of rules

5. after_enter_to_pos / enter -- Right after entering in position

6. after_indicators / inds -- after initialization of indicators

7. unrealized_money_last / unreal -- unrealized_money_last variable should be defined as unrealized pnl for each leg

8. before_enter_to_pos / bexit -- before enter to position

9. before_exit_from_pos / bexit -- before exit from position

10. after_exit_from_pos / exit -- after exit from position

11. data -- initalization of tables and derivative datasets

12. start_cycle / on_start -- at the start of cycle before any update of spread}
}
\description{
Adds variables to strategy
}
