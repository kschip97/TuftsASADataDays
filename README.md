# TuftsASADataDays
Educational Materials from Weekly Tufts-ASA Data Days workshops. Files include R scripts, R markdown files, and output from said files. Most of the data used in these demonstrations are built-in to R packages, so the upload of data files is not necessary for the git repository.

# Brief overview of scripts

## Data_Days_Automate_Reading_Files.R

often times in data science, we have data spread across many files in a directory. This script aims to teach individuals how to read in those files efficiently, so that they don't have to manually type in `read_csv()` fifty or more times. This script also goes into dataset splitting, and using `data.table` to combine lists into datasets with `rbindlist()`

## Data_Days_bigmac_20200114.R

a simple data exploration script for showing some easy ways to get to know your data in R using the TidyTuesday Big Mac dataset.

## Data_Days_coffee_20210111.R

Similar to bigmac, in that this file was not used for a live demonstration, but rather as a script for showing how to do some exploratory data analysis. This script used data from TidyTuesday's week examining coffee ratings.

## Data Days_COVID_Data_20200118.R

This script goes over how to get data directly from a github repository using read_csv from the readr package. This script also covers how to pivot time-series data from wide to long format for better analysis. This workshop tried to introduce the concept of tidy data, but not enough time was allotted to go in detail

## Data_Days_dplyr_Kenya.R

This script acted as a walkthrough of dplyr data manipulation functions like mutate, select, filter, arrange, summarise, and group_by. I use these functions every time I code, so I thought a dedicated session to dplyr would help student's confidence with working with and manipulating data into the form they need for analysis

## Data_Days_functions.R

This script introduces individuals to function syntax in R and how to use functions to improve your data science workflows, and create reproducible scripts for your peers to be able to follow along

## Data_Days_functions_contflow.R

This script introduced the concept of control flow, using if, else if, else, break, next, and other control flow statements. This script was an extension of Data_Days_functions, except that this script included a function for a coin flip simulator, showing how control flow can be used to automate probabilistic inference

## Data_Days_functions_contflow_scratchwork.R

Scratchwork file for functions and controlflow scripts. I'm considering deleting the file, but for now it remains in the repository.

## Data_Days_ggplot_IntroForEDA.R

This script is an introduction to using ggplot for exploratory data analysis. The script starts by introducing the layer-based concept of ggplot, and then goes into detail with aesthetics, especially groupings, fill, and color, to show characteristics of different divisions in data. Finally, I introduced faceting through `facet_wrap()` and `facet_grid()` to stratify visual EDA by variables and groups.

## Data_Days_Rmarkdown_session.Rmd

This Rmarkdown file goes through how to use Rmarkdown to create aesthetic reports to integrate your code, data visualizations, equations, and annotations into programmatic reports. This script covers headings, lists, tables, LaTeX equations, html hyperlink embedding, Rmd setup, chunk options, themes, code highlighting, code chunks and output, figure annotation, and html jpg, png, and video embedding.

## Data_Days_pivoting_plastics.R

This script specifically focuses on pivoting data to get that data into the shape that you need for analysis. This script uses `pivot_longer()` and `pivot_wider()` to manipulate data, and applications of pivoted data in analysis and ggplot are utilized, as pivoting is usually necessary to make faceted plots.

