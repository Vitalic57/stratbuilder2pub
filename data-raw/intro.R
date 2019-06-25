# INTRO TO R
# If you want to execute some code in R document, highlight the code and enter ctrl+Enter
# If you do it without selecting the code, then one line or bunch of code will be executed

# The can include comments. Comments started with # character. The part of the line after # is ignored
# by interpreter.


# some basic arithmetic operations
1 + 1 # adding
1.5 - 4 # Subtracting
4 * 5 # multiplication
1 / 5 # division
4^pi # power, pi is 3.14...
exp(5) # exponent 

# Exercise: compute e^pi, where e := 2.71....

# relational operations

5 > 0   # greater than   
4 < 2   # less than
4 >= 4  # greater or equal than 
3 <= 6  # less or equal
5 == 5  # equal
4 == 5  
3 != 4  # not equal
4 != 4  

# Exercise: check what is greater e^pi or pi^e. 


# logical operations

# for scalars
5 > 0 && 5 < 10 # conjuction or logical AND
5*4 < 0 || 7^2 == 49 # disjuction or logical OR
# for vectors use & and |


# assigning variables is done with help of <- operator
a <- 1

# here can be used '=' sign for assigning, but it is not recommended
a = 1

# = is used for passing arguments to a function

# to print variable enter command print(a) or select it and ctrl+Enter
print(a)
a

# basic class character, numeric, logical

s <- "this is a character variable" # character
class(s)                           # print type of a variable

a <- 5                              # numeric 
class(a)
as.character(a)                     # convert numeric to character

b <- TRUE                           # logical variable. Logical variables can be TRUE of FALSE 
as.numeric(b)                       # as numeric they equal to 1 and 0 correspondingly


# There are some basic structures array, list, matrix and data.frame

# Arrays 
arr <- c(1, 2, 5) # creating an array with elements 1, 2, 5. c symbol combines elements into an array
arr[2]            # accessing to an element of the array by index. First element has index 1

arr <- 1:10 # fillin an array with 1, 2...10. Arrays must have elements of the same type
arr

# arrays can be multiplied, divided, added, powered, substracted by a variable or another array with the same length

arr + 5
arr / 2
arr + 6:15


r <- rep(5, 10) # repeat 5 10 times 
r

seq(0, 1, 0.1) # create array which contains numbers from 0 to 1 with 0.1 step

# Exercise: create an array which contains all 1)odd 2)even integers  from 1 to 100.  


l <- list(a = 1, s = "str", q = c(1, 3, 4)) # Lists can have elements of any type. Here is how we construct lists. 

l
l$a      # Accessing to an element of the list.  
l[["a"]] # Another way to access 
l[[1]]   # Another way to access
l[1]        # Returning a list which consists of first element
l[c(1, 2)]  # Returning a list which consists of first and second element of the origin list

# Types of elements of a list don't have to be the same

# Matrices
A = matrix(            # creating matrix 
  c(2, 4, 3, 1, 5, 7), # the data elements 
  nrow=2,              # number of rows 
  ncol=3,              # number of columns 
  byrow = TRUE)        # fill matrix by rows 

A[1, 2]                # returning an element of the matrix which resides in first row and second column
A[1, ]                 # first row  
A[, 2]                 # second column 

nrow(A)                # Getting number of rows 
ncol(A)                # Getiing number of columns
dim(A)                 # Getting dimension of matrix


# Merging matrices. rbind and cbind

B = matrix(c(1:6), nrow = 2, byrow = TRUE)

rbind(A, B) # Appending rows of A and B 
cbind(A, B) # Appending columns of A and B

# Exercise: creat matrix with C which consists of the first row of A and the second row of B


emp.data <- data.frame(
  emp_id = c(1:5), 
  emp_name = c("Rick","Dan","Michelle","Ryan","Gary"),
  salary = c(623.3,515.2,611.0,729.0,843.25), 
  
  start_date = as.Date(c("2012-01-01", "2013-09-23", "2014-11-15", "2014-05-11",
                         "2015-03-27")),
  stringsAsFactors = FALSE
)

# Accesing to an elements of data.frame is the same as for lists
# and also as for matrices
emp.data$emp_id
emp.data[,1]
		
print(emp.data$emp_name) 

# if statement introduce condition
a <- 0
if(a == 0){
  print('a is equal to 0 here')
}else if(a == 1){
  print('Now a is equal to 1')
}else{
  print('a is not 0 or 1')
}

# try diffrent a # 1, 2

# loops


# repeat an action while the condition is TRUE
i <- 0
while (i < 7) {
  i <- i + 1
}

# repeat an action for every number from 1:10 (1, 2, ..., 10)
s <- 0
for (i in 1:10) {
  s <- s + 1
}
s

# about functions

# creating function which adds two values. This function could be applied to any params which can be added 


func <- function(param1, param2) { 
  return(param1 + param2)
}

func(1, 2)
func(1.5, 6)
func(param1 = 4, param2 = -1)

func(param1 = TRUE, TRUE)

func(c(1, 2, 3), c(5, 6, 3)) # adding two vectors




# there are some basic functions
a <- c(1, 2, 5)
sum(a)    # summation
min(a)    # minimum
max(a)    # maximum
length(a) # length of array, list ...

# sapply function applies a function to each element of the vector or list

x <- c(1, 4, 9, 16, 25)
sapply(X = x, FUN = is.numeric) # create a vector which contains TRUE if corresponding value is numeric

# you can customize the function
sapply(X = x, FUN = function(x) {return(x^0.5)}) # create vector of square roots 

# lapply does the same, but returns a list not vector


# about quote
# quote() returns an expression: an object that represents an action that can be performed by R

x <- 2
e <- quote(x^2 + 5*x + 4)
e
eval(e) # performing an expression

# Ploting 

x <- seq(0, pi, 0.001) 

plot(x, sin(x)) # plotting sin graph
plot(sin(x))    # plotting without x-axis. X-axis will be filled by 1, 2 ... integers


# Packages. There are large numbers of packages in Rfor statistical analysis and data science.
# In order to use them, first of all, you have to install them
# For example, let's install xts package
install.packages("xts")

# Then do 
library(xts)

# Dates
# There is date class for dates representation in R.

Sys.Date() # Getting current date
Sys.time() # Getting current time

date <- as.Date("2000-01-01", format = "%Y-%m-%d")
date
date + 1
date - 1


# xts library. xts library provide for uniform handling of R's different time-based data classes

xts1 <- xts(x=1:10, order.by=Sys.Date()-1:10) # create xts object
xts1
index(xts1)  # returning dates
coredata(xts1) # returning data

data <- rnorm(5) # 5 random variables from standard normal distribution 
dates <- seq(as.Date("2017-05-01"), length=5, by="days")
xts2 <- xts(x=data, order.by=dates)

# you can plot xts objects
plot(xt2)

# Exercise create an xts object index of which is every day in 2018 and consist of a columns "day"
# which consist of ordinal number of the day
            
            
            
# Help. If you are looking for some information about a package or function (or another object) just use help or '?'
# For example
help(sum)
?sum
package?xts

# References 
# http://adv-r.had.co.nz/
# https://www.r-project.org/about.html
# R Cookbook: Proven Recipes for Data Analysis, Statistics, and Graphics (O'reilly Cookbooks)
# Online courses in DataCamp and Coursera


