## Data_Days_pivoting_plastics.R ##
## Created by: Kees Schipper ##
## Date Created: 2021-01-26 ##
## Date Last Updated: 2021-01-26 ##

rm(list = ls())
# Load Packages -----------------------------------------------------------

if (!require(tidytuesdayR)){install.packages("tidytuesdayR")}
if (!require(tidyverse)){install.packages("tidyverse")}
if (!require(Hmisc)){install.packages("Hmisc")}
if (!require(gapminder)){install.pacakges("gapminder")}

library(tidytuesdayR)
library(tidyverse)
library(Hmisc)
library(gapminder)


# load tt data ------------------------------------------------------------

tt_load(2021, week = 5)
# if you want to read in the data manually:
plastics <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-26/plastics.csv')


# Preliminary data examination --------------------------------------------

# create some intuitive variable labels:
labs <- c(country = "country", year = "year", parent_company ="source of plastic", 
          empty = "Cat left empty count", 
          hdpe = "High density polyethylene count",
          ldpe = "low density polyethylene count",
          o = "category marked other count", 
          pet = "polyester plastic county",
          pp = "polypropylene count",
          ps = "polystyrene count",
          pvc = "PVC plastic count",
          grand_total = "Count of all types of plastics",
          num_events = "Numb of counting events",
          volunteers = "Numb of volunteers")
# apply labels
label(plastics) = as.list(labs[match(names(plastics), names(labs))])

# summary stats
summary(plastics)

# pivoting our variables: pivot_wider() -----------------------------------
vignette("pivot")

# in making a summary table, we are going to reduce our data to one numerical variable
gapminder_red <- gapminder %>%
  select(country:lifeExp, -continent)

# This code creates a summary table by country, of life expectancy per country by year
gapminder_wide <- gapminder_red %>%
  pivot_wider(names_from = "year",
              values_from = "lifeExp")

# test <- gapminder %>%
#   select(-pop, -gdpPercap) %>%
#   pivot_wider(names_from = c('year', 'country'),
#               values_from = 'lifeExp')


plast_wide <- plastics %>%
  select(country:parent_company, grand_total) %>%
  pivot_wider(names_from = 'country',
              values_from = 'grand_total')

# pivot_longer(): More useful for making tidy data ------------------------

Fremont <- read_csv("https://data.seattle.gov/api/views/65db-xm6k/rows.csv?accessType=DOWNLOAD")

Fremont_long <- Fremont %>%
  pivot_longer(cols = c("Fremont Bridge Total":"Fremont Bridge West Sidewalk"),
               names_to = "Bridge",
               values_to = "traffic") %>%
  mutate(Bridge = str_remove_all(Bridge, "Fremont Bridge ")) %>%
  separate(Date, into = c("Date", "time"), sep = " ", extra = "merge") %>%
  mutate(Date = as.Date(Date, "%m/%d/%Y")) %>%
  group_by(Date, Bridge) %>%
  dplyr::summarize(day_traffic = sum(traffic, na.rm = F))

Fremont_long %>%
  ggplot(aes(x = Date, y = day_traffic, color = Bridge)) +
  geom_line(size = 1) +
  xlim(c(as.Date('2014-01-01'), as.Date('2016-01-01'))) +
  labs(title = "Traffic on brige by date", x = "date", y = 'traffic level (people)') +
  theme(legend.position = 'none') +
  theme_classic()

ggplot(data = Fremont_long, aes(x = Date, y = day_traffic, color = Bridge)) +
  geom_point() +
  geom_smooth(method = 'loess', span = .10)


# Back to plastics: let's see if we can make some summaries ---------------

plastics %>%
  count(country, year, wt = grand_total, sort = T) %>%
  mutate(year = factor(year)) %>%
  ggplot() +
  geom_bar(aes(x = reorder(country, n), y = n, fill = year), stat = 'identity') +
  coord_flip() +
  labs(title = "Plastics by country by year", x = "xlab", y = "ylab")

