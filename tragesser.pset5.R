# code for pset5, write ups found in rmd/pdf files:

library(foreign)
library(tidyverse)
library(modelsummary)
library(formatR)

setwd("/Users/connortragesser/Documents/Political Science Coursework Readings/Quant 1/Problem Set 5/")

# Question 3

ace <- read.dta("acemoglu.dta")
head(ace)
development <- read.dta("development.dta")
head(development)


# seems like we need to merge by name + year, which seems to uniquely identify the rows 

# lets make sure we have unique obs in the var we're interested in: 

ace$counter <- 1

ace2 <- ace %>% 
  group_by(ccodealp_year) %>% 
  mutate(num = sum(counter))

ace_examine <- ace2 %>% 
  filter(num != 1)
head(ace_examine)

# clear coding issue with ccodealp_year (looks like lots of instances where they forgot to make the year 12 instead of 11) , going to try cname_year instead 

ace3 <- ace %>% 
  group_by(cname_year) %>% 
  mutate(num = sum(counter))

ace_examine2 <- ace3 %>% 
  filter(num != 1)
head(ace_examine2)

# cname_year is unique for acemoglu, ensure it is for development

development$counter <- 1

development2 <- development %>% 
  group_by(cname_year) %>% 
  mutate(num = sum(counter))

development_examine <- development2 %>% 
  filter(num != 1)
head(development_examine)

# cname_year is unique for development, remove datasets no longer needed 

rm(ace_examine, ace_examine2, ace2, ace3, development2, development_examine)

# clean up datasets of variables that will be repeats between the two datasets (basically, further details that are captured by cname_year that aren't actually needed any more)

ace <- ace %>% 
  select(c(cname_year, ajr_settmort))

development <- development %>% 
  select(c(cname_year, icrg_qog, wdi_gdpc))

merged_ace_development <- full_join(ace, development, by = "cname_year")

head(merged_ace_development)

merged_ace_development %>% 
  select(c(ajr_settmort, icrg_qog, wdi_gdpc)) %>% 
  summary()


ggplot(merged_ace_development, aes(x = icrg_qog, y = log(wdi_gdpc))) + 
  geom_point() +
  geom_smooth(method = lm) +
  theme_bw() +
  ylab("Log GDP Per Capita (PPP)") +
  xlab("Quality of Government") +
  labs(subtitle = "Linear function of X on Y overlaid")


ggplot(merged_ace_development, aes(y = log(wdi_gdpc), x = ajr_settmort)) + 
  geom_point() +
  geom_smooth(method = lm) +
  theme_bw() +
  ylab("Log GDP Per Capita (PPP)") +
  xlab("Settler Mortality") +
  labs(subtitle = "Linear function of X on Y overlaid")

ggplot(merged_ace_development, aes(x = ajr_settmort, y = icrg_qog)) + 
  geom_point() +
  geom_smooth(method = lm) +
  theme_bw() +
  xlab("Settler Mortality") +
  ylab("Quality of Government") +
  labs(subtitle = "Linear function of X on Y overlaid")




