# CODE REPRODUCTION

setwd("/Users/connortragesser/Documents/Political Science Coursework Readings/Quant 1/Problem Set 1/")
getwd()
library(foreign)
library(tidyverse)
rm(list = ls()) 

# import data
fh_data <- read.dta("fh.dta")
head(fh_data)

# 23rd observation of CL in the dataset
print(fh_data$fh_cl[23])

# Filter for Germany
fh_dta_germany <- fh_data %>% 
  filter(ccodewb == "DEU")

# Print the PR variable for Germany
print(fh_dta_germany$fh_pr)


# generate new variable 

fh_data_addvar <- fh_data %>% 
  mutate(fh_clpr = fh_cl + fh_pr)

# Print 87th observation
print(fh_data_addvar$fh_clpr[87])

# Print 4th observation of the added variable
print(fh_data_addvar$fh_clpr[4])

# Print the entire 4th observation to try to diagnose issue
print(fh_data_addvar[4,])

# Generate new dataset based on prescribed filter

vector_cl_trim <- fh_data %>% 
  filter(fh_cl > 3 | is.na(fh_cl))

# Count number of observations
print(count(vector_cl_trim))
# Add the fh_cl variable
print(sum(vector_cl_trim$fh_cl, na.rm = TRUE))


