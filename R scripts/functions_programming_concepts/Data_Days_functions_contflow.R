## Data_Days_functions_contflow.R ##
## Created by Kees Schipper ##
## Date Created: 2021-02-07 ##
## Date last updated: 2021-02-07 ##


rm(list = ls())
dev.off()
# load packages -----------------------------------------------------------
library(tidyverse)
library(tidytuesdayR)


# read in TT data ---------------------------------------------------------

tt_week6 <- tt_load(2021, week = 6)


# make a function for cleaning up data in all datasets --------------------

rm_noalnum <- function(df){
  
  temp_clean <- df %>%
    mutate(across(where(is.character), .fns = ~str_remove_all(.x, "[^[:alnum:]]")),
           across(where(is.character), .fns = ~ifelse(.x == "", NA, .x)),
           across(where(is.character), .fns = ~as.numeric(.x)))
  
  keepind <- which(apply(temp_clean, 1, function(x) any(!is.na(x))))
  output <- temp_clean[keepind, ]
  
  return(output)
  
}

# funny enough we have our first example of using a for loop.
for (i in 1:length(tt_week6)){
  tt_week6[[i]] <- rm_noalnum(tt_week6[[i]])
  assign(names(tt_week6)[i], tt_week6[[i]])
}


# function syntax ---------------------------------------------------------
# name of function, followed by function(). a, b, and c are called "arguments" to the function

myfunc <- function(a, b, c){
  
  output <- a + b - c
  return(output)
  # use the "return" function to tell the function what value you want outputted. You can
  # only use the return function once per function, but if you want multiple things returned,
  # you can compile them in a list.
}

myfunc(5, 6, 3)


# create a function to solve quadratic equations
quadfunc <- function(a, b, c){
  
  if (is.nan(sqrt(b**2 - 4*a*c))){
    stop("There are no real roots for this quadratic")
  }
  
  x_pos <- (-b + sqrt(b**2 - 4*a*c))/2*a
  x_neg <- (-b - sqrt(b**2 - 4*a*c))/2*a
  
  return(c(x_pos, x_neg))
}
#x^2 + 3x + 1
quadfunc(1, 3, 1)


# for loop syntax ---------------------------------------------------------

numbloops <- 1000 # define how many loops you want to go through

for(i in 1:numbloops){
  print(i)
}

# can use for loops to build vectors from the bottom-up
emptyvec <- c()

# let's time this for loop to see how long it takes:
ptm1 <- proc.time()
for (i in 1:numbloops){
  emptyvec <- c(emptyvec, i)
}
proc.time() - ptm1

print(emptyvec)
# not empty anymore!

# however, this looping process is slow. It is much faster to make a vector and then
# fill it with values:
emptyvec2 <- vector(mode = "double", length = numbloops)
print(emptyvec2)

# lets time this for loop as well
ptm2 <- proc.time()
for (i in 1:numbloops){
  emptyvec2[i] <- i
}
proc.time() - ptm2

# control flow ------------------------------------------------------------

# make a for loop to keep only even numbers

# create a vector of random integers from 0-1000
in_vec = sample.int(1000, size = 1000)
even_numbs <- c()

# use a for loop with control flow to keep only even integers
for (i in 1:length(in_vec)){
  
  if(in_vec[i] %% 2 == 0){
    
    even_numbs <- c(even_numbs, in_vec[i])
    
  } else {
    even_numbs <- even_numbs
  }
    
}
  


# Let's combine a function and for loop to simulate coin flips ------------


Sim_Coin_Flip <- function(numbflips, seed){
  
  
  # set.seed(sample(1:10000, 1)) 
  # numbflips = as.numeric(readline("How many times do you want to flip: "))
  
  
  # set seed for reproducibility. If you use the same seed number, 
  # you should get the exact same results every time. Hence why R uses
  # pseudo random number generators
  prob <- runif(numbflips, min = 0, max = 1) 
  # sample from a uniform distribution from -1 to 1
  heads <- 0
  # need to initialize heads so R knows what we refer to in for loop. Same for tails
  tails <- 0
  theta <- vector(mode = 'double', length = numbflips) 
  # can also initialize vectors. R has a cool capability where you can append things
  # to vectors, which we will use to build our sets of heads and tails in our for loop
  probheads <- vector(mode = 'double', length = numbflips)
  # another vector initialized

  ptm <- proc.time()
  for (i in 1:length(prob)){
    
    if (prob[i] > 0.5){ # if probability is greater than 0.5, we take that as a heads
      heads <- heads + 1
      theta[i] <- 1
      # probheads <- c(probheads, heads/i)
      probheads[i] <- heads/i
    } else if (prob[i] < 0.5){ # if probability is less than 0.5, we take that as a tails
      tails <- tails + 1
      theta[i] <- 0
      # probheads <- c(probheads, heads/i)
      probheads[i] <- heads/i
    } else { # if prob is neither, we skip that segment of for loop...not important for our purposes
      next
    }
    
  }
  print(proc.time() - ptm)
  
  
  # here we have a vector of length 1000 which shows the cumulative count of heads
  cumul_theta = cumsum(theta)
  
  
  # sometimes plotting can take a long time if you have a lot of points. As an exercise, introduce
  # some control flow here which gives the user an option of whether or not they want to plot
  # when the number of flips is greater than 100,000:
  
  
  plot(cumul_theta, type = "o", col = "blue", main = "Cumulative number of 'heads' in our trial")
  # here we can plot the probability of heads over time
  plot(probheads, type = "o", col = "blue", main = "probablity of flipping heads vs trials",
       log = "x", ylim = c(0, 1))
  abline(h = 0.50, col = "red")
  
  return(list(heads = heads, tails = tails,
              prop_heads = heads/numbflips,
              prop_tails = tails/numbflips,
              cumul_theta = cumul_theta,
              theta = theta))
}

# let's try out our simulation
x <- Sim_Coin_Flip(numbflips = 5000, seed = 100)
