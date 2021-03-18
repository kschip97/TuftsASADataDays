## Data_Days_functions.R ##
## Created by Kees Schipper ##
## Date Created: 2021-02-10 ##
## Date last updated: 2021-02-10 ##

library(tidyverse)


# what are functions? -----------------------------------------------------
# functions are useful ways to condense repetitive code into single lines, taking
# what are called 'arguments,' which are the pieces separated by commas inside of a 
# function's parentheses.
# if we've ever coded, we've used some sort of function
x <- 1:20
mean(x)
median(x)
min(x)

# mean, median, and min are all functions, where x is the argument. The arguments are
# what a function operates on, and are the only thing variable within a function. 


# Making a function in R --------------------------------------------------
# functions in R use the following syntax
# func_name <- function(arguments){
#   function body
# }
# you can then run the function by typing func_name(arguments)

# let's try making a function that does a bunch of arithmetic oprations
arithmetic <- function(a, b){
  add = a + b
  subtract = a - b
  divide = a / b
  modulus = a %/% b
  multiply = a * b
  exponent = a ** b
  
  return(c(add, subtract, divide, modulus, multiply, exponent))
  # the return argument let's you specify what you want to get out of the funciton.
  # if you don't use this argument, the function won't return anything (except if you
  # use plot or print)
}

arithmetic(5, 6)

larithmetic <- function(a, b){
  add = a + b
  subtract = a - b
  divide = a / b
  modulus = a %/% b
  multiply = a * b
  exponent = a ** b
  
  # you can also return elements as a structured list with titles
  return(list(add = add, subtract = subtract, divide = divide, modulus = modulus,
              multiply = multiply, exponent = exponent))
}
larithmetic(5, 6) 
# this will give your output more meaning, and this is also how functions like
# lm() and summary() return output.


# functions are great for data cleaning! ----------------------------------
# getting data
Fremont <- read_csv("https://raw.githubusercontent.com/DataScienceWorks/PredictingBicycleTraffic/master/data/FremontBridge.csv")

# let's say we wanted to make a function to automate the process of cleaning data sets.
# working with variables is a little different but still somewhat simple. You can use
# tidyverse functions within a function to help with the cleaning process.
# let's start by making raw code, and then transforming it into a function:

# we want to (1) pivot fremont bridges to be in one column, (2) separate date/time
# into 2 columns, (3) group by day to get daily statistics, and (3) summarize the grouped
# df

Fremont %>%
  rename(East = `Fremont Bridge East Sidewalk`,
         West = `Fremont Bridge West Sidewalk`) %>%
  mutate(Total = East + West) %>%
  pivot_longer(cols = c("East", "West", "Total"),
               names_to = "sidewalk",
               values_to = "traffic") %>%
  extract(Date, into = c('date', 'time'), regex = '(\\d{2}/\\d{2}/\\d{4}) (\\d{2}:\\d{2}:\\d{2})') %>%
  mutate(date = as.Date(date, '%d/%m/%Y'),
         weekday = weekdays(date)) %>%
  group_by(sidewalk, weekday) %>%
  summarise(total = sum(traffic, na.rm = T),
            mean = mean(traffic, na.rm = T))

# now say we want to make this into a function, because we want to look at stats grouped
# by different times. copy and paste the above code into the function syntax, and edit the 
# variables that you might want to change. For now, let's just change the grouping variables


Fremontsumm <- function(df, ...){
  cleaned <- df %>% # maybe you named your Fremont df differently
    rename(East = `Fremont Bridge East Sidewalk`,
           West = `Fremont Bridge West Sidewalk`) %>%
    mutate(Total = East + West) %>%
    pivot_longer(cols = c("East", "West", "Total"),
                 names_to = "sidewalk",
                 values_to = "traffic") %>%
    extract(Date, into = c('date', 'time', 'AM_PM'), regex = '(\\d{2}/\\d{2}/\\d{4}) (\\d{2}:\\d{2}:\\d{2}) (\\w{2})') %>%
    mutate(date = as.Date(date, '%d/%m/%Y'),
           weekday = weekdays(date),
           hour = str_extract(string = time, pattern = "\\d{2}"))
  
  summ <- cleaned %>%
    group_by(...) %>% # make the grouping variables mutable in your function
    summarise(total = sum(traffic, na.rm = T),
              mean = mean(traffic, na.rm = T))
  
  return(list(summary = summ, data = cleaned)) # return a cleaned data set and a grouped summary
}

weekday_summ <- Fremontsumm(Fremont, weekday, time)
time_of_day <- Fremontsumm(Fremont, hour, AM_PM) 
AM_PM_traffic <- Fremontsumm(Fremont, AM_PM, sidewalk)

