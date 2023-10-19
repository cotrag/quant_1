# Problem set 7 - Connor Tragesser

# See write up/discussants in pdf/rmd

library(foreign)
library(tidyverse)
library(modelsummary)
library(formatR)
library(data.table)
knitr::opts_chunk$set(tidy.opts = list(width.cutoff = 60), tidy = TRUE)
knitr::opts_chunk$set(echo = TRUE)

setwd("/Users/connortragesser/Documents/Political Science Coursework Readings/Quant 1/Problem Set 7/")

getwd()
rm(list = ls()) 

set.seed(2453)


## Problem 1

### Part A
pnorm(6, mean = 4, sd = 2) - pnorm(2, mean = 4, sd = 2)

### Part B
(60-66)/4

## Problem 2
### Part B
4/(100^(1/2))


## Problem 4

### Gailmard 7.4

n = 30

draws_normal <- replicate(10000, rnorm(n))
draws_df <- as.data.frame(draws_normal)

draws_df <- draws_df %>% 
  mutate(across(everything(), ~var(.), .names = "sample_var_{.col}")) %>% 
  mutate(across(V1:V10000, ~var(.) * n-1, .names = "other_calc_{.col}"))

# now get only the calculations we're interested in, and only need 1 row 

calcs <- draws_df %>% 
  select(starts_with("other_calc_")) %>% 
  slice(1:1) %>% 
  data.table::transpose()

ggplot(aes(x = V1), data = calcs) +
  geom_histogram(aes(y = ..density..)) +
  stat_function(fun = dchisq, args = list(df = n-1)) +
  theme_bw() +
  xlab("Calculation") +
  ylab("Density") +
  ggtitle("Distribution of Calculation, X^2 Overlaid")

rm(draws_df)
rm(calcs)


### Gailmard 7.5

draws_uniform <- replicate(10000, runif(n, min = -3, max = 3))
draws_df <- as.data.frame(draws_uniform)

draws_df <- draws_df %>% 
  mutate(across(everything(), ~var(.), .names = "sample_var_{.col}")) %>% 
  mutate(across(V1:V10000, ~(var(.) * n-1)/((1/12)*((3 + 3)^2)), .names = "other_calc_{.col}"))

# now get only the calculations we're interested in, and only need 1 row 

calcs <- draws_df %>% 
  select(starts_with("other_calc_")) %>% 
  slice(1:1) %>% 
  data.table::transpose()

ggplot(aes(x = V1), data = calcs) +
  geom_histogram(aes(y = ..density..)) +
  stat_function(fun = dchisq, args = list(df = n-1)) +
  theme_bw() +
  xlab("Calculation") +
  ylab("Density") +
  ggtitle("Distribution of Calculation, X^2 Overlaid")

rm(draws_df)
rm(calcs)

### Gailmard 7.6

draws_df <- as.data.frame(draws_normal)

draws_df <- draws_df %>% 
  mutate(across(everything(), ~(mean(.)-0)/(sd(.)/(n-1)^(1/2)), .names = "t_{.col}"))

# now get only the calculations we're interested in, and only need 1 row 

calcs <- draws_df %>% 
  select(starts_with("t_")) %>% 
  slice(1:1) %>% 
  data.table::transpose()

ggplot(aes(x = V1), data = calcs) +
  geom_histogram(aes(y = ..density..)) +
  stat_function(fun = dt, args = list(df = n-1)) +
  theme_bw() +
  xlab("T") +
  ylab("Density") +
  ggtitle("Distribution of T, Student's T PDF Overlaid")

rm(draws_df)
rm(calcs)

### Gailmard 7.7

draws_df <- as.data.frame(draws_uniform)

draws_df <- draws_df %>% 
  mutate(across(everything(), ~(mean(.)-0)/(sd(.)/(n-1)^(1/2)), .names = "t_{.col}"))

# now get only the calculations we're interested in, and only need 1 row 

calcs <- draws_df %>% 
  select(starts_with("t_")) %>% 
  slice(1:1) %>% 
  data.table::transpose()

ggplot(aes(x = V1), data = calcs) +
  geom_histogram(aes(y = ..density..)) +
  stat_function(fun = dt, args = list(df = n-1)) +
  theme_bw() +
  xlab("T") +
  ylab("Density") +
  ggtitle("Distribution of T, Student's T PDF Overlaid")

rm(draws_df)
rm(calcs)

## Problem 5
years <- c(1968:1978)
sui_rate <- c(10.7, 11.2, 11.3, 11.9, 12.1, 12.0, 12.3, 12.4, 12.7, 13.6, 11.8)
ue <- c(3.6, 3.7, 4.8, 6.0, 5.7, 4.8, 5.7,  8.4, 7.8, 7.2, 5.7)
q5_dataset <- cbind(years, sui_rate, ue)

q5_dataset <- as.data.frame(q5_dataset)

### Part A

# Begin by calculating the means of the variables of interest 
mean_suicide <- (10.7 + 11.2 + 11.3 + 11.9 + 12.1 + 12.0 + 12.3 + 12.4 + 12.7 + 13.6 + 11.8)/11
mean_ue <- (3.6 + 3.7 + 4.8 + 6.0 + 5.7 + 4.8 + 5.7 +  8.4 + 7.8 + 7.2 + 5.7)/11

# working out deviation and deviation squared for the X
q5_dataset$ue_deviation <- q5_dataset$ue - mean_ue
q5_dataset$ue_deviation_squared <- q5_dataset$ue_deviation^2
q5_dataset$ue_deviation_squared
ue_deviation_squared_sum = 4.681322314 + 4.258595041 + 0.928595041 + 0.055867769 + 0.004049587 + 0.928595041 + 0.004049587 + 6.950413223 + 4.146776860 + 2.063140496 + 0.004049587

# working out deviation and deviation squared for the Y
q5_dataset$suicide_deviation <- q5_dataset$sui_rate - mean_suicide
q5_dataset$suicide_deviation_squared <- q5_dataset$suicide_deviation^2
q5_dataset$suicide_deviation_squared
suicide_deviation_squared_sum = 1.69 + 0.64 + 0.49 + 0.01 + 0.01 + 0.00 + 0.09 + 0.16 + 0.49 + 2.56 + 0.04

# getting value that multiplies the deviation for X and Y, for the numerator for (3)

q5_dataset$deviations_mult = q5_dataset$ue_deviation * q5_dataset$suicide_deviation
q5_dataset$deviations_mult
deviations_mult_sum = 2.812727273 + 1.650909091 + 0.674545455 + -0.023636364 + -0.006363636 + 0.000000000 + -0.019090909 + 1.054545455 + 1.425454545 + 2.298181818 + 0.012727273
r = (deviations_mult_sum)/((ue_deviation_squared_sum * suicide_deviation_squared_sum)^(1/2))
sd_suicide <- (suicide_deviation_squared_sum/(11 - 1))^(1/2)
sd_ue <- (ue_deviation_squared_sum/(11-1))^(1/2)
beta_1 = r*(sd_suicide/sd_ue)
beta_1
beta_0 = mean_suicide - (beta_1 * mean_ue)
beta_0

### Part B

regression_manual <- function(x, y, N) {
  beta_1 = cor(x, y)*(sd(y)/sd(x))
  beta_0 = mean(y) - (beta_1 * mean(x))
  paste("The coefficient for X is", beta_1, "and The intercept is", beta_0)
}

regression_manual(q5_dataset$ue, q5_dataset$sui_rate, 11)

### Part C

ggplot(q5_dataset, aes(x = ue, y = sui_rate)) +
  geom_point() +
  theme_bw() +
  xlab("Unemployment Rate (%)") +
  ylab("Suicides per 100,000") +
  geom_abline(aes(intercept = 9.629817, slope = 0.4112305))


### Part D

9.629817 + (0.4112305*5.5)






