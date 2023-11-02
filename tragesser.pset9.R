# Write ups, collaborators, etc. are in the rmd/pdf

library(foreign)
library(tidyverse)
library(modelsummary)
library(formatR)
library(data.table)
library(readxl)
library(gmodels)
library(gtsummary)
library(gt)
library(flextable)
library(kableExtra)

setwd("/Users/connortragesser/Documents/Political Science Coursework Readings/Quant 1/Problem Set 9/")

rm(list = ls()) 

set.seed(2453)

## Gailmard 8.4
ppois(5, lambda = 3, lower.tail = FALSE)

## Gailmard 8.5
lamda_dist <- rpois(40, lambda = 5)

### Part A
t.test(lamda_dist, mu = 5)

### Part B
t.test(lamda_dist, mu = 8)

## Problem 3
culture <- read.dta("apsrfinaldata.dta")
head(culture)

reg_model_culture <- lm(pg ~ betweenstd + lngdpstd, data = culture)
summary(reg_model_culture)
mapper = list("betweenstd" = "between group inequality", "lngdpstd" = "stnd. value of the log of PCGDP")

modelsummary(reg_model_culture, stars = TRUE,  coef_map = mapper)

## Problem 4
mar <- read.dta("MAR_2006.dta")
head(mar)


mar <- mar %>% 
  mutate(POLGR_str = ifelse(POLGR == 0, str_c("No political", "grievances expressed", sep = "\n"), 
                            ifelse(POLGR == 1, str_c("Political grievances focused on", 
                                                     "ending discrimination", sep = "\n"),
                                   ifelse(POLGR == 2, str_c("Political grievances focused on", 
                                                            "creating or strengthening", "remedial policies", sep = "\n"),
                                          ifelse(POLGR == 3, 
                                                 str_c("Political grievances focused on", 
                                                       "creating or strengthening", "autonomous status", sep = "\n"),
                                                 ifelse(POLGR == 4, str_c("Political grievances focused on", 
                                                                          "creating separate state for group", 
                                                                          "or revanchist change in borders", 
                                                                          sep = "\n"), "")))))) %>% 
  mutate(EXECREP_str = ifelse(EXECREP == 0, str_c("No group representation", "in central government", sep = "\n"), 
                              str_c("Group representation", "in central government", sep = "\n"))) %>% 
  mutate(GROUPCON_str = ifelse(GROUPCON == 0, "Widely dispersed", 
                               ifelse(GROUPCON == 1, "Primarily urban or minority in one region",
                                      ifelse(GROUPCON == 2, "Majority in one region, others dispersed",
                                             ifelse(GROUPCON == 3, "Concentrated in one region", "")))))

mar$POLGR_str <- factor(mar$POLGR_str, levels = unique(mar$POLGR_str[order(mar$POLGR)]), ordered = TRUE)
mar$EXECREP_str <- factor(mar$EXECREP_str, levels = unique(mar$EXECREP_str[order(mar$EXECREP)]), ordered = TRUE)
mar$GROUPCON_str <- factor(mar$GROUPCON_str, levels = unique(mar$GROUPCON_str[order(mar$GROUPCON)]), ordered = TRUE)


bar_chart_maker <- function(var, title, xlabel) {
  ggplot(aes(x = {{var}}), data = mar) +
    geom_bar() +
    theme_bw() +
    ggtitle(title) +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
    xlab(xlabel) +
    ylab("Count")
}

bar_chart_maker(POLGR_str, "Group Political Grievance", "Group Political Grievance")
bar_chart_maker(EXECREP_str, "Group Representation in Executive Branch of Central Government", 
                "Group Representation in Executive Branch of Central Government")
bar_chart_maker(GROUPCON_str, "Group Geographical Concentration", "Group Geographical Concentration")



chisq.test(x = mar$EXECREP, y = mar$POLGR)

freq_table <- mar %>% 
  select(POLGR_str, EXECREP_str) %>% 
  tbl_cross(percent = "row", label = list(POLGR_str ~ "Group Political Grievance",
                                          EXECREP_str ~ "Group Rep in Executive Branch")) %>%
  as_flex_table(width = 10)
freq_table

props <- mar %>% 
  group_by(EXECREP) %>% 
  summarise(num = n()) %>% 
  mutate(prop = num/sum(num)) %>% 
  select(EXECREP, prop) %>% 
  spread(EXECREP, prop) 

exp_table <- mar %>% 
  group_by(POLGR_str) %>% 
  summarise(total_no_rep = n()) %>% 
  mutate(total_with_rep = total_no_rep)

exp_table$total_no_rep = exp_table$total_no_rep * props$`0`
exp_table$total_with_rep = exp_table$total_with_rep * props$`1`
exp_table$total = exp_table$total_no_rep + exp_table$total_with_rep
exp_table

