# see write up and discussants on rmd or pdf

setwd("/Users/connortragesser/Documents/Political Science Coursework Readings/Quant 1/Problem Set 11/")
library(foreign)
library(tidyverse)
library(modelsummary)
library(formatR)
library(data.table)
library(readxl)
library(scales)

getwd()
rm(list = ls()) 

set.seed(12345)

# Problem 1
# Gailmard 9.5 Part C
pchisq(78, 1, lower.tail = FALSE)

# Problem 2
subprime <- read.dta("subprime.dta")
head(subprime)

# Part A
num_subprime <- subprime %>% 
  filter(high_rate == 1)

true_subprime_rate = nrow(num_subprime)/nrow(subprime)

graph_data = subprime
graph_data$high_rate = as.factor(graph_data$high_rate)

graph_data <- graph_data %>% 
  mutate(high_rate_str = ifelse(high_rate == 1, "Subprime", "Prime"))

ggplot(aes(x= high_rate_str), data = graph_data) +
  geom_bar(aes(y = (after_stat(count))/sum(after_stat(count))), 
           fill = "cadetblue") +
  scale_y_continuous(labels=percent) +
  theme_bw() +
  ylab("Count") +
  xlab("Borrower Status") +
  ggtitle("Proportion of Prime/Subprime Borrowers in Cape Coral and Fort Meyers")

# Part B
sample_subprime <- sample_n(subprime, 250)

sample_num_subprime <- sample_subprime %>% 
  filter(high_rate == 1)

sample_prop_subprime <- nrow(sample_num_subprime) / nrow(sample_subprime)

ci_maker <- function(prop, n, z, level) {
  lower_bound = -z * sqrt((prop * (1 - prop))/ n)
  upper_bound = z * sqrt((prop * (1 - prop))/ n)
  paste("The CI is ", 100 * round(prop + lower_bound, 4), "% to ", 
        100 * round(prop + upper_bound, 4), "% at the ", 
        level, " level", sep = "")
}

ci_maker(sample_prop_subprime, nrow(sample_subprime), 0.674, "50%")
ci_maker(sample_prop_subprime, nrow(sample_subprime), 1.96, "95%")
ci_maker(sample_prop_subprime, nrow(sample_subprime), 2.576, "99%")

# Part C

lower_bound = -1.96 * sqrt((sample_prop_subprime * (1 - sample_prop_subprime))/
                             nrow(sample_subprime))
upper_bound = 1.96 * sqrt((sample_prop_subprime * (1 - sample_prop_subprime))/
                            nrow(sample_subprime))
paste("The margin of error, based on the 95% confidence level, is: ", 
      round(100 * (((sample_prop_subprime + upper_bound) - 
                      (sample_prop_subprime + lower_bound))/2), 3)
      , "%", sep = "")
