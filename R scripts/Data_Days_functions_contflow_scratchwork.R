## Data_Days_functions_contflow_scratchwork.R ##
## Created by Kees Schipper ##
## Date Created: 2021-02-10 ##
## Date last updated: 2021-02-10 ##

# simulate a coin flip

x <- 1:10

quadeq <- function(arg1, arg2, arg3){
  
  # what is your function doing
  
}


printnumb <- function(numb){
  print(numb + 5)
  
  return(numb)
}

x <- printnumb(5)


# control flow ------------------------------------------------------------

# if ("cond is true"){
#   do this
# } else if ("this is true"){
#   do this
# } else {
#   "everything else failed so just do this"
# }

y <- runif(100, -1, 1)


if (x > 0) {
  print('heads')
  print(x)
} else if (x < 0){
  print('tails')
  print(x)
}


for (ind in 1:length(y)){
  print(y[ind])
}


outcome <- c()
outcome_vec <- vector(mode = "character", length = length(y))
for (i in 1:length(y)){
  cat("iteration", i, "\n")
  
  if (y[i] > 0) {
    print('heads')
    outcome[i] <- 'heads'
    # outcome <- c(outcome, 'heads')
    # print(y[i])
  } else if (y[i] < 0){
    print('tails')
    outcome[i] <- 'tails'
    # outcome <- c(outcome, 'tails')
    # print(y[i])
  }
  
}

y[1]
y[50]
y[100]
