# Script for PS2 - Answers to the questions are in the write up found in the RMD or HTML files 


# Prepare workspace 

library(foreign)
library(tidyverse)
library(modelsummary)
library(formatR)

setwd("/Users/connortragesser/Documents/Political Science Coursework Readings/Quant 1/Problem Set 2/")

getwd()
rm(list = ls()) 

# read in data
fh_data <- read.dta("fh.dta")

# problem 2 pt A

# add IQR and SD
summary(fh_data$fh_cl)
summary(fh_data$fh_pr)
count(fh_data)
non_na_counter <- function(x){
  z <- fh_data %>% 
    dplyr::filter(!is.na({{x}})) %>% 
    count()
  return(paste("We have", z$n, "non-missing values for the column",  enexpr(x)))
}

non_na_counter(fh_cl)
non_na_counter(fh_pr)

# Problem 2 pt B

fh_data <- fh_data %>% 
  mutate(fh_clpr = (fh_cl + fh_pr)/2) %>% 
  mutate(fh_status = ifelse(fh_clpr >= 1 & fh_clpr < 3, 1, 
                            ifelse(fh_clpr >= 3 & fh_clpr <= 5, 2,
                                   ifelse(fh_clpr > 5 , 3, NA))))

fh_data$fh_status <- as.factor(fh_data$fh_status)
levels(fh_data$fh_status) <- c("Free", "Partly Free", "Not Free")
summary(fh_data$fh_status)

# Percentage Table, not including NAs
prop.table(table(fh_data$fh_status))

# Percentage Table, including NAs
test <- fh_data %>% 
  group_by((fh_status)) %>% 
  count()

test <- test %>% 
  mutate(pct = n/sum(n))

test$pct = test$n/sum(test$n)
print(test)

# Problem 3

cces_data <- read.dta("cces.dta")

# make a new age variable, based on birthyr and today's date
cces_data$age <- year(today()) - cces_data$birthyr

# nothing looks out of place, stewart and oreilly are 1-7 as expected 
summary(cces_data$age)
summary(cces_data$pid7)
summary(cces_data$stewart)
summary(cces_data$oreilly)

graph_hist_func <- function(variable, xaxis) {
  ggplot(cces_data, aes(x = {{variable}})) + 
    geom_histogram(fill = "blue", binwidth = .5) +
    xlab(xaxis) +
    ylab("Count") +  
    theme_bw()
}

graph_hist_func(stewart, xaxis = "Stewart")
graph_hist_func(oreilly, xaxis = "Oreilly")
graph_hist_func(pid7, xaxis = "Party ID")

ggplot(cces_data, aes(x = age)) + 
  geom_density(color = "blue") +
  xlab("Age") + 
  theme_bw()


liner_model = lm(stewart ~ pid7, data = cces_data)
modelsummary(liner_model, coef_rename = c("pid7" = "Party Identification"), stars = TRUE)


