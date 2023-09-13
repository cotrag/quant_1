# script for PS3 - Answers and write up are in RMD/PDFs

library(foreign)
library(tidyverse)
library(modelsummary)
library(formatR)

setwd("/Users/connortragesser/Documents/Political Science Coursework Readings/Quant 1/Problem Set 3/")

getwd()
rm(list = ls()) 

# Read in data
qog <- read.dta("QoG_2010.dta")
head(qog)

# Problem 2, Part A
124/164

# Problem 2, Part B
(64+29)/164

# Problem 2, Part C
29/40

# Problem 2, Part D
64/(64+29)

# Problem 2, Part E
1-.756
(.244*.725)/.567

# Problem 5, part A
boxplot(qog$undp_gdp, xlab = "per capita GDP ($)", ylab = "Amount")

# Problem 5, part B

ggplot(qog, aes(x=fh_status, y=undp_gini)) + 
  geom_boxplot() + 
  xlab("Freedom Index") +
  ylab("Gini Coefficient") +
  theme_bw()

# Problem 5, part C

ggplot(qog, aes(x = undp_gdp, y = wdi_gris)) +
  geom_point() +
  theme_bw() +
  xlab("GDP Per Capita") +
  ylab("% Girls to Boys Enrolled in Schools")

