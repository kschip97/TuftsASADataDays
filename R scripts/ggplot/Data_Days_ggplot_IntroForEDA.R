## Data_Days_ggplot_IntroForEDA.R ##
## Programmer: Kees Schipper ##
## Date created: 2021-02-22 ##
## Date updated: 2021-02-22 ##
# Notes: The purpose of this script is to introduce the ggplot package. This 
# introduction will go through creating the base ggplot, attaching data to your
# plot, setting aesthetics, working with basic color schemes and palettes,
# facetting, facet grids, plotting multiple variables, and the geometries
# for line plots, smoothers, scatterplots, boxplots, bar plots, histograms,
# jittering, heatmaps. Culminating in plotting a 
# regression model with confidence intervals

# link to notebook corresponding to this script:
# https://rpubs.com/YTSchipper23/730627

# link to this script in box
# https://tufts.box.com/s/s64kjowoiedhs5kdowhvfnie2z89kes1

# link to data file for this folder
# https://tufts.box.com/s/wrg79ai07bexubck20xtdhufwvhgkjfn



# clear environment and plotting window -----------------------------------

rm(list = ls())
dev.off()
# set working directory to the Data Days output folder, or wherever your data is stored
setwd("C:/Users/keess/Box/ASA Share/ASA 2020/ASA Meetings/Data Days Spring 2021/Data Days output")

library(tidyverse)

# The Diamonds dataset ---------------------------------

df <- diamonds
names(df)

# simple scatterplots of diamond price by carat
df %>%
  ggplot(aes(x = carat, y = price)) +
  geom_point()

# maybe we can make points transparent so we see where they concentrate
p <- df %>%
  ggplot(aes(x = carat, y = price)) +
  geom_point(alpha = 0.1)

# there seems to be a trend. We could look at the trend with a geom_smooth
p_smooth <- p + geom_smooth(method = 'loess', se = F)
p_smooth

# in addition, we can break this plot down into different 'facets' by another variable
p_smooth + facet_wrap(~cut)

# here we split our plot up by different cuts. I've also shown the layered nature of ggplot,
# where graphing elements are added one on top of the other. If we were to write that
# whole plot out using code (and adding labels) it would look like this:
df %>%
  ggplot(aes(x = carat, y = price)) +
  geom_point(alpha = 0.1) +
  geom_smooth(method = 'loess', se = F) +
  facet_wrap(~cut) +
  labs(title = "Price vs. Carat, Faceted by Diamond Cut",
       x = 'Carat', y = 'Price', subtitle = "ggplot tutorial for EDA",
       caption = "By Kees Schipper")


# Now let's do the same layered process for a boxplot ---------------------

# start by simply looking at the distribution of price in our entire dataset
df %>%
  ggplot(aes(y = price)) +
  geom_boxplot()

# clearly not very informative. Let's split price into cut
df %>%
  ggplot(aes(x = cut, y = price)) +
  geom_boxplot()

# this gives us a little more information, but we can improve on our boxplot
# aesthetics with notches and outlier colors
df %>%
  ggplot(aes(x = cut, y = price)) +
  geom_boxplot(notch = TRUE, outlier.color = 'blue', fill = 'grey50', color = 'blue')

# we can increase the dimensions accross which we visualize our data by adding
# facets again.

df %>%
  ggplot(aes(x = cut, y = price)) +
  geom_boxplot(outlier.color = 'blue') +
  facet_wrap(~clarity)

# here our notches aren't especially useful, and we also see that labels tend to overlap
# we could try flipping our axes
df %>%
  ggplot(aes(x = cut, y = price)) +
  geom_boxplot(outlier.color = 'blue') +
  facet_wrap(~clarity) +
  coord_flip()

# this is still a little confusing but at least the labels don't overlap
# finally, we can add labels. And let's keep the coordinates as they were originally,
# but we can use the ggplot theme to tilt our x labels
df %>%
  ggplot(aes(x = cut, y = price)) +
  geom_boxplot(outlier.color = 'blue') +
  facet_wrap(~clarity) +
  theme(axis.text.x = element_text(hjust = 0.5, angle = 45)) +
  labs(title = "Price of Different Diamond Cuts, Stratified by Clarity")

# I almost forgot! We can also add summary statistics to our boxplot with stat_summary()
# we can also look at adding jitter so that we can see individual data points in our plots
df %>%
  ggplot(aes(x = cut, y = price)) +
  geom_boxplot(outlier.color = 'blue') +
  stat_summary(fun = 'mean', col = 'red', size = 0.2) +
  geom_jitter(color = 'brown', alpha = 0.10) +
  facet_wrap(~clarity) +
  theme(axis.text.x = element_text(hjust = 1, angle = 45)) +
  labs(title = "Price of Different Diamond Cuts, Stratified by Clarity")


# Now we can look at histograms and density plots -------------------------

# histograms are somewhat easy to implement in ggplot
df %>%
  ggplot(aes(x = price)) +
  geom_histogram(bins = 200) # specify number of bins to increase detail in distribution

# like with other plots, we can color, facet, etc... and we can also stack
# different categories using the position argument
df %>%
  ggplot(aes(x = price, fill = cut)) +
  geom_histogram(bins = 200, position = 'stack')

# this visualization isn't super useful, as we can't see the proportions super
# well. We can change this by using the position = 'fill' option
df %>%
  ggplot(aes(x = price, fill = cut)) +
  geom_histogram(bins = 200, position = 'fill')

# now we can see the relative proportions of what diamonds make up which price
# however, the jitteriness of this plot still isn't fantastic. This is where
# density plots come in
df %>%
  ggplot(aes(x = price, fill = cut)) +
  geom_density(alpha = 0.25, color = "black")

# here we can visualize the smoothed shape of multiple distributions. However,
# it's hard to see the different distributions when they're one on top of the other
df %>%
  ggplot(aes(x = price, fill = cut)) +
  geom_density(position = 'stack', color = "black") +
  facet_wrap(~cut)

# now we can see the proportions of different diamonds making up different prices
# but there's still a better way if we're only interested in proportions
df %>%
  ggplot(aes(x = price, fill = cut)) +
  geom_density(position = 'fill', color = "black")

# Interestingly, all diamond cuts have a dip in the price range of around $4000-5000.
# Maybe this is because jewlers tend to start charging at around $5000, and don't bother
# with mid-range prices.

# A geom halfway between histograms and the densit plot is the 'geom_freqpoly' which
# makes a frequency polygon with jagged edges matching a given number of bins
df %>%
  ggplot(aes(x = price, color = cut)) +
  geom_freqpoly(bins = 50, size = 1) +
  scale_color_hue()


# some other geoms that may be worth trying -------------------------------

# geom hex
df %>%
  ggplot(aes(x = carat, y = price)) +
  geom_hex() +
  scale_fill_viridis_c()

# geom_density_2d
df %>%
  ggplot(aes(x = carat, y = price)) +
  geom_density_2d()

# geom_density_2d_fill
df %>%
  ggplot(aes(x = carat, y = price)) +
  geom_density_2d_filled() +
  geom_density_2d(color = "black") +
  ylim(0, 6000) +
  xlim(0, 1)

# violin plots
df %>%
  ggplot(aes(x = cut, y = price, fill = cut)) +
  geom_violin() +
  facet_wrap(~clarity)

?geom_hline
?geom_vline

# for all of the geoms, you can type geom_<TAB> and scroll through the autocomplete
# to see what is available
# In my opinion, the R package "viridis" provides some of the best color schemes taht you could
# use for (1) visibility in both color and grayscale, and (2) colorblind friendliness.
# check out the documentation on the viridis color palettes' usage below:
# https://ggplot2.tidyverse.org/reference/scale_viridis.html

# read in data ------------------------------------------------------------
# load in COVID data for all counties across the united states
load('COVID_master_20200218.RData')

# select a county or state that you're interested in:
MACovid <- COVID_master %>%
            filter(state == "Massachusetts" & Population > 0) %>%
            group_by(date) %>%
            summarise(across(where(is.numeric), ~sum(.x, na.rm = T))) %>%
  select(date, cases, deaths:daily_deaths_100k)
# we now have data for the entire state of Massachusetts. If you want to work with
# a different state, change Massachusetts to whatever state that interests you!


# simple visualizations of the data ---------------------------------------
# let's examine a scatterplot of cases in Massachusetts compared to deaths

# this is about as simple a ggplot as you can get. 
ggplot(data = MACovid, aes(x = daily_cases, y = daily_deaths)) +
  geom_point()

# your ggplot specifies global options, so you don't have to specify aesthetics
# in your added geometries. This has benefits and drawbacks...

# We can also store this as a ggplot object and add to it
p <- ggplot(data = MACovid, aes(x = daily_cases, y = daily_deaths))
# p for plot
p # gives us an empty plot with scales corresponding to our data


# adding to a ggplot object -----------------------------------------------

# let's add some points, and a line showing the relationship of our points:
p +
  geom_point() +
  geom_smooth(se = T, span = 0.2, method = "loess", color = 'red') +
  labs(title = "COVID Deaths vs. Cases")

# geom_smooth does a simple loess smoother by default, over our data. Note:
# you don't actually need to plot your points to get a smoother, R knows what
# data you are using, so the smoother is only dependent on your data, not the
# plots that you have in ggplot beforehand

# methods of geom_smooth include linear models (lm) loess, and generalized 
# additve models (gam)


# boxplots ----------------------------------------------------------------
# I'm interested in differences of case and death counts by weekday. Let's get a
# variable for that

MAWeekday <- MACovid %>%
  mutate(weekday = factor(weekdays(date)))

summary(MAWeekday$weekday) # now each observation corresponds to a weekday

# let's plot case counts by weekday
MAWeekday %>%
  ggplot(aes(x = weekday, y = daily_cases)) +
  geom_boxplot() +
  stat_summary(fun = 'mean', color = 'red', geom = 'point')
# now we have boxplots of cases by weekday. We can also add a mean value indicator
# with stat_summary()

# If we like color, we could color weekdays differently
MAWeekday %>%
  ggplot(aes(x = weekday, y = daily_cases, fill = weekday)) +
  geom_boxplot(outlier.alpha = 0) +
  geom_jitter(color = "blue", alpha = 0.2) +
  stat_summary(fun = 'mean', color = 'red', geom = 'point')

# note the difference between specifying color in the aes() function vs in the general
# gemetry. aes() maps a data value to an inherent aspect of ggplot. Therefore, each
# different value of weekday returns a different color. If you specify color or fill
# outside of aes, that singular color is applied to all of the fills and/or colors for 
# that geometry.


# some other interesting geometries ---------------------------------------
# let's accumulate data by week first
week <- 1
daycount <- 0
MAWeekday$week <- 0
for (i in 1:nrow(MAWeekday)){
  print(week)
  MAWeekday$week[i] <- week
  daycount <- daycount + 1
  
  if (daycount == 7){
    
    daycount <- 1
    week <- week + 1
    
  }
}
# create a count of weeks

MAWeek <- MAWeekday %>%
  select(cases, deaths, daily_cases:week) %>%
  group_by(week) %>%
  summarise(across(is.numeric, sum))

# line graphs
MAWeek %>%
  ggplot(aes(x = week)) +
  geom_line(aes(y = daily_cases), size = 1, col = "blue") +
  geom_line(aes(y = daily_deaths), size = 1, col = "red") +
  labs(title = "Comparing Cases and Deaths in Massachusetts")

# geom histogram
MACovid %>%
  ggplot(aes(x = daily_cases)) +
  geom_histogram(bins = 70)

# can even stack histograms 
MACovid %>%
  pivot_longer(cols = c('daily_cases', 'daily_deaths'),
               names_to = 'outcome',
               values_to = 'values') %>%
  ggplot() +
  geom_histogram(aes(x = values, fill = outcome), position = "stack", bins = 30, col = "blue") 

# not the greatest visualization. Let's use a frequency polygon to compare distributions
MACovid %>%
  pivot_longer(cols = c('daily_cases', 'daily_deaths'),
               names_to = 'outcome',
               values_to = 'values') %>%
  ggplot() +
  geom_freqpoly(aes(x = values, color = outcome), size = 1, bins = 50) +
  labs(title = "Now we can see both distributions")


# can even use a density plot as an overlay -------------------------------

MAWeekday %>%
  ggplot() +
  geom_density(aes(x = daily_cases, group = weekday, color = weekday), size = 1) +
  labs(title = "Daily case count density functions by weekday--Hard to see")


# Let's facet! ------------------------------------------------------------
# faceting lets us separate our plots by a factor or character value
MAWeekday %>%
  ggplot() +
  geom_density(aes(x = daily_cases, group = weekday, color = weekday), size = 1) +
  facet_wrap(~weekday) +
  labs(title = "Faceted case counts by weekday--much clearer!!")

# let's look at other variables
MAWeekday %>%
  ggplot() +
  geom_density(aes(x = parks, group = weekday, color = weekday), size = 1) +
  facet_wrap(~weekday) +
  labs(title = "Faceted case counts by weekday--much clearer!!") +
  theme(legend.position = "none") # after faceting, we don't need a legend anymore!


# Let's look at a heatmap/density map -------------------------------------

MAWeekday %>%
  ggplot(aes(x = daily_cases, y = daily_deaths)) +
  geom_density_2d_filled() +
  geom_density_2d(color = 'black') +
  geom_point(color = 'black', alpha = 0.35) +
  scale_fill_brewer(palette = "Reds") +
  ylim(0, 100) +
  xlim(0, 3000) +
  facet_wrap(~weekday)

# Final note on faceting --------------------------------------------------
# if you want to facet your data by a category, your data needs to be in the
# following format (but not in this order):
# x column | y column | faceting column
# because of this, often times you will have to pivot your data for faceting.
# Let's do this to look at the relationships between deaths, cases, and mobility
# over time

facet_data <- MAWeekday %>%
  pivot_longer(cols = c('retail_rec':'daily_deaths'),
               names_to = 'vars',
               values_to = 'values') %>%
  select(date, vars, values, weekday)

facet_data %>%
  ggplot(aes(x = date, y = values)) +
  geom_line(aes(color = vars))

# see...not the most visible data. Much easier to facet

facet_data %>%
  ggplot(aes(x = date, y = values)) +
  geom_line(aes(color = vars)) +
  facet_wrap(~vars)

# you can also facet by multiple variables. You just need to use `facet_grid` and
# specify the column facet, and the row facet
facet_data %>%
  ggplot(aes(x = date, y = values)) +
  geom_line(aes(color = vars)) +
  facet_grid(cols = vars(vars), rows = vars(weekday), scales = 'free_y') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




# Further resources -------------------------------------------------------

# for more on visualizations in R, I always point people to the R graph gallery:
# https://www.r-graph-gallery.com/
# if you have an idea for a visualization, there is likely some code already in the
# gallery that can get you started on making your final visualization. If you're stuck
# I recommend checking it out.
