## Data_Days_dplyr_Kenya.R ##
## Created by: Kees Schipper ##
## Created on: 2021-01-24 ##
## Last Updated: 2021-01-25 ##

setwd("C:/Users/keess/Box/ASA Share/ASA 2020/ASA Meetings/Data Days Spring 2021/Data Days output")

# load in libraries -------------------------------------------------------
if (!require(tidyverse)){install.packages("tidyverse")}
if (!require(tidytuesdayR)){install.packages("tidytuesdayR")}

library(tidyverse)
library(tidytuesdayR)


# load in tidytuesday dataset ---------------------------------------------

Kenya_All <- tt_load(2021, week = 4)
Kenya_All

House <- Kenya_All$households
Crops <- Kenya_All$crops
Gender <- Kenya_All$gender

str(House)
str(Crops)
str(Gender)

x <- summary(House)
summary(Crops)
summary(Gender)

# Introduction to dplyr functions -----------------------------------------

?magrittr::`%>%`
?dplyr::select
?dplyr::filter
?dplyr::mutate
?dplyr::arrange
?dplyr::group_by
?dplyr::summarize
?dplyr::rename


# %>% or the pipe operator ------------------------------------------------
# f(x) is the same thing as x %>% f()

x <- c(1, 2, 3, 4, 5)
sum(x)
x %>% sum()


# select and helper functions ---------------------------------------------
# look at the column names in a data set
names(House)

# basic use of the select function. Get columns for pop and households
select(House, Population, NumberOfHouseholds)

# same as the above code but with a pipe
House %>% 
  select(County, Population)

# viewing but not storing output. Using `:` to specify a range of columns
House %>%
  select(County:NumberOfHouseholds) %>% View()

# storing your piped operations into a new data frame
House_cleaned <- House %>%
  select(County:NumberOfHouseholds)

# negative selection: read as "I want to remove columns 1 through 3 from my data set
House %>%
  select(-c(1:3))

# use pipes to string together multiple functions (rm avghousesize and rename population)
House %>%
  select(-AverageHouseholdSize) %>%
  rename(Pop = Population)

# reorder rows by specifying from left-to-right the order of rows that you want
House %>% select(Population, everything())

# selects all columns that match but don't throw an error if one entry doesn't match
House %>% select(any_of(c("County", "notacolumn", "Population")))

?starts_with
?ends_with
?contains
?matches # regular expression matching...maybe a little too complicated to explain
?num_range
?everything
?last_col
?all_of
?any_of

?where # a lot of really cool use-cases with this function. where allows the user to select
       # columns based on a function. Function must result in TRUE or FALSE outputs to be used


# filtering: similar to select but for rows -------------------------------
# boolean/relational operators:
# ?`<` #less thaopulationn
# ?`>` #greater than
# ?`<=` #less than or equal to
# ?`>=` #greater than or equal to
# ?`==` #is equal to
# ?`!=` #is not equal to
# ?between # if you want a faster way of specifying a range
# ?`|` # bitwise "or"
# ?`&` # bitwise "and"
# ?`||` # Or for control flow
# ?`&&` # And for control flow
# can filter data by boolean operations. With filter(), referring to column names
# directly within the filter function
House_filter <- House %>%
  filter(Population < 605415 & !is.na(Population))

# can also compare variables in a dataset to set values 
MedPop <- median(House_filter$Population)

House_filter %>%
  filter(Population < MedPop)


# for || and &&, if the first condition is not met, the if statement returns a FALSE
# immediatesly, whereas with & and |, both elements of the boolean statement are checked




# mutate: for creating new variables --------------------------------------

# create new variables based on old ones
House %>%
  mutate(NumbHouse = Population/AverageHouseholdSize)

# base R version: less neat IMO
House$NumbHouse = House$Population/House$AverageHouseholdSize

# summarize: for creating aggregate summaries of data ---------------------
library(moments) # for skewness and kurtosis


House_new <- House[2:nrow(House), ]

House_new %>%
  summarise(sd_pop = sd(Population),
            kurt_pop = kurtosis(Population),
            med_house = median(NumbHouse),
            avg_hsize = mean(AverageHouseholdSize),
            stderr_pop = sd(Population)/n(),
            N = n(),
            nMiss = sum(is.na(Population)),
            Ncomp = sum(!is.na(Population)))

?complete.cases()
?min()
?max()
?quantile()


# group_by: for making summaries and vars group-specific ------------------
group_data <- starwars

# group_by creates implicit groups in your data to perform group-wise operations
# remember: group_by doesn't perform any operations on our data, but the groupings
# are applied to operations later on 

group_data %>%
  group_by(eye_color) %>%
  summarise(grp_mass = mean(mass, na.rm = T))

group_data %>%
  group_by(eye_color, skin_color) %>%
  summarise(grp_mass = mean(mass, na.rm = T))



# Now let's play around with the Kenya data -------------------------------

# need to watch out for counties "Homa Bay" "Nairobi", "Tana River", "Trans Nzoia",
# "West Pokot", "Uasin Gishu"

# examine the names of each dataset
names(Crops)
names(Gender)
names(House)

# looks like the variable "County" could be a key to let us combine tables
Crops$SubCounty
Gender$County
House$County
# there are some county names that do not match

# need to adjust the county names to be able to merge

# list of functions we are using
?trimws()
?str_to_title()
?ifelse()
?grepl()

Crops_clean <- Crops %>%
  rename(County = SubCounty) %>%
  mutate(County = trimws(str_to_title(County)),
         County = ifelse(grepl("Nairobi", County), "Nairobi", County),
         County = ifelse(grepl("Homa", County), "Homa Bay", County),
         County = ifelse(grepl("Tana", County), "Tana River", County),
         County = ifelse(grepl("Trans", County), "Trans Nzoia", County),
         County = ifelse(grepl("Westp", County), "West Pokot", County),
         County = ifelse(grepl("Uasing", County), "Uasin Gishu", County))

Gender_clean <- Gender %>%
  mutate(County = trimws(str_to_title(County)),
         County = ifelse(County == "Total", "Kenya", County),
         County = ifelse(grepl("Nairobi", County), "Nairobi", County),
         County = ifelse(grepl("Homa", County), "Homa Bay", County),
         County = ifelse(grepl("Tana", County), "Tana River", County),
         County = ifelse(grepl("Trans", County), "Trans Nzoia", County),
         County = ifelse(grepl("Westp", County), "West Pokot", County),
         County = ifelse(grepl("Uasing", County), "Uasin Gishu", County))

House_clean <- House %>%
  mutate(County = trimws(str_to_title(County)),
         County = case_when(
           grepl("Nairobi", County) ~ "Nairobi",
           grepl("Homa", County) ~ "Homa Bay",
           grepl("Tana", County) ~ "Tana River",
           grepl("Trans", County) ~ "Trans Nzoia",
           grepl("Westp", County) ~ "West Pokot",
           grepl("Uasing", County) ~ "Uasin Gishu",
           TRUE ~ County
         ))

# now we can merge
all_kenya <- full_join(Crops_clean, Gender_clean, by = "County") %>%
  full_join(House_clean) %>%
  select(-Total)


# A.1 set a working directory ----------------------------------------------

?setwd()
?getwd()

write_csv(all_kenya, file = "Combined_Kenya_Ag.csv")

rm(list = ls())

kenya <- read_csv("Combined_Kenya_Ag.csv")

library(haven) # for reading in unique data types



