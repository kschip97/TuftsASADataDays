## Data_Days_Automate_Reading_Files.R ##
## Created by: Kees Schipper ##
## Date Created: 02/15/2021 ##
## Last Updated: 02/15/2021 ##

rm(list = ls())
dev.off()

# set up a folder in which you are comfortable with storing 50 files.
# it should essentially be a folder that you can clear after the exercise
setwd("C:/Users/keess/Box/ASA Share/ASA 2020/ASA Meetings/Data Days Spring 2021")

library(tidytuesdayR)
library(tidyverse)
library(data.table)


# tidytuesday data --------------------------------------------------------


animal_outcomes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-21/animal_outcomes.csv')
animal_complaints <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-21/animal_complaints.csv')
brisbane_complaints <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-21/brisbane_complaints.csv')

# create pseudo data to read in -------------------------------------------
# also write data into csv files in 
# 
# for (i in 1:50){
#   x <- sample(x = 50:100, size = 100, replace = TRUE)
#   z <- seq(1, 3, by = (3-1)/99)
#   alpha = 5.72
#   beta_1 = 0.24
#   beta_2 = 0.2
#   beta_3 = 50
# 
#   y <- alpha + beta_1*x + beta_2*x**2 + beta_3*sin((2*pi*z)/1) + rnorm(n = 100, mean = 0, sd = 40)
#   gen_data <- data.frame(
#     x = x,
#     y = y,
#     z = z
#     )
# 
#   write_csv(x = gen_data, paste0("Data Days output/genned_data/sample_data_", i, "-2021-02-15.csv"))
# }


# Show how to read in these files with a for loop -------------------------
# create a base path as a string variable
basepath <- "Data Days output/genned_data/"

# get a list of files in the directory you are looking at. You can store this list as a 
# variable!
files_to_read <- list.files(basepath)

# before we start our for loop, we should make a list to store our data in.
# this will increase efficiency
store_dat <- vector(mode = "list", length = length(files_to_read))

# To read in files, we need multiple parts:
# 1. The base directory that all of your files are contained in +
# 2. The name of the files that you are going to read--which we have.
# 3. paste these together, and you have a path that you can insert into read_csv

# use the paste0 function to paste together these text components...
# example with the first file in 'files_to_read'
paste0(basepath, files_to_read[1])
# spits out this lovely path to our first file:
# "Data Days output/genned_data/sample_data_1-2021-02-15.csv"

# now we can apply this to a for loop, and loop over all of the file names in 
# 'files_to_read'

for (i in 1:length(files_to_read)){
  read_csv(paste0(basepath, files_to_read[i])) 
  # use the index to iterate through each file name. basepath is a constant for this
  # whole process
}

# unfortunately, this loop reads, but doesn't store any of our data. To store the data,
# put each csv file in a list:
for (i in 1:length(files_to_read)){
  store_dat[[i]] <- read_csv(paste0(basepath, files_to_read[i])) 
}
# now you can access each file as an element of a list!
store_dat[[1]]

# Show how to combine files by column with data.table ---------------------
#rbindlist
# data.table has an excellent function that you can use to combine similar elements
# from a list into one data frame. Note: this function will work easiest if all of the
# column names are the same across datasets
library(data.table)

combined_data <- rbindlist(store_dat, use.names = T)
# now all of your data from the list should be combined into one!
plot(combined_data$x, combined_data$y)


# Say that each dataset is different and you want -------------------------
# to read things in separately

# remove the file extension, or non-identifying text from file names
ind_readin <- str_remove(files_to_read, "-.*") 
# the above string uses something called a "regular expression" to remove the date and
# file extensions from all of the strings in files_to_read. The string "-.*" means dash and
# any character after the dash, and str_remove is removing those characters. Check out the output:
ind_readin


# use a for loop to read in files to sep datasets -------------------------


for (i in 1:length(ind_readin)){
  # use the "assign()" function to assign 
  temp <- read_csv(paste0(basepath, files_to_read[i]))
  assign(ind_readin[i], temp)
  
  # here's what's going on:
  # 1. assign is a function used to assign an object to a character string
  # 2. the string provided by ind_readin[i] is the name of the dataframe being created
  # 3. the read_csv is utilizing the basepath and files_to_read we made earlier to read
  #    files 1-by-1, and assign them to ind_readin[i]
  
}

# for example, here's what the components of the first iteration of the for loop are
ind_readin[1]
paste0(basepath, files_to_read[1])

# since we used files_to_read to make our object names, each value of ind_readin should
# correspond to each value of files_to_read in the for loop. Now you have 50 files in your
# environment (at least).


# bonus material: splitting your data -------------------------------------

# R has a useful function called split(), which takes a df and a factor as argument,
# and splits the data along that factor

split_dat <- split(combined_data, cut(combined_data$x, breaks = c(50, 60, 70, 80, 90, 100)))

# since we don't have a factor, we can make one with the cut function. The cut function takes
# a vector, some break points, and creates a factor variable with levels based off of the break
# points

cut(combined_data$x, breaks = 5, labels = c("low", "low-med", "med", "med-high", "high"))
# here, cut creates our factor variable with which we can divide our data

range(split_dat[[1]]$x)
range(split_dat[[2]]$x)
range(split_dat[[3]]$x)



# exploratory data analysis -----------------------------------------------

rm(list = ls(pattern = "sample"))

library(psych)
library(zoo)




comp_clean <- animal_complaints %>%
                rename(animal = `Animal Type`,
                       complaint = `Complaint Type`,
                       date = `Date Received`,
                       el_div = `Electoral Division`)

comp_clean %>%
  count(animal, sort = T)

comp_clean %>%
  count(complaint, sort = T)

comp_clean %>%
  count(Suburb, sort = T)

# can we make a time series with the date variable?
library(lubridate)
unique(comp_clean$date)

month_complaint <- comp_clean %>%
  extract(date, into = c('month', 'year'), regex = '([A-Za-z]*) (\\d{4})') %>%
  group_by(month)

month_complaint %>%
  count(complaint, sort = T) %>%
  ggplot(aes(x = reorder(month, n), y = n, fill = complaint)) +
  geom_bar(position = 'stack', stat = 'identity') +
  coord_flip() +
  scale_fill_brewer(palette = 'Dark2') +
  labs(title = "Animal Complaints by Type and Month",
       y = "Number of Complaints",
       x = "Month from greatest to least counts")

month_complaint %>%
  count(animal, sort = T) %>%
  ggplot(aes(x = reorder(month, n), n, fill = animal)) +
  geom_bar(position = 'stack', stat = 'identity') +
  coord_flip() +
  scale_fill_brewer(palette = 'Dark2') +
  labs(title = "Animal Complaints by Animal and Month",
       y = "Number of Complaints",
       x = "Month from Greatest to Least Counts")


# time series of complaints by month --------------------------------------

complaint_ts <- month_complaint %>%
  ungroup(month) %>%
  mutate(month_num = match(month, month.name),
         day = 1,
         date = make_date(year = year, month = month_num, day = day)) %>%
  select(-month, -year, -day) %>%
  group_by(date) %>%
  count(complaint, animal)

complaint_ts %>%
  filter(complaint == "Noise") %>%
  ggplot(aes(x = date, y = n, color = complaint)) +
  geom_line() +
  geom_smooth(span = 0.2, method = "loess") +
  labs(y = "Number of Complaints",
       x = "Date",
       title = "Time Series of Complaints")


# kz filter code ----------------------------------------------------------
# extra:
# test_ts <- complaint_ts %>%
#   group_by(date) %>%
#   summarise(counts = sum(n))
# 
# 
# x <- stats::filter(test_ts$counts, filter = c(1/3, 1/3, 1/3), sides = 2)
# 
# plot(x)
# lines(test_ts$counts, col = 'red')
