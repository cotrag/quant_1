# write up and discussants are in rmd/pdf

# set up
pacman::p_load("foreign", "tidyverse", 
               "modelsummary", "formatR", 
               "data.table", "readxl", 
               "scales", "lmtest", 
               "sandwich", "kableExtra", 
               "gmodels", "gtsummary", 
               "stargazer", "car", 
               "broom", "Hmisc", 
               "AER", "MatchIt", 
               "rgenoud", "modelsummary", "Matching")

setwd("/Users/connortragesser/Documents/Political Science Coursework Readings/Quant 1/Problem Set 13/")

getwd()
rm(list = ls()) 

set.seed(413947)


## Problem 2
dta <- read.dta("bhw_data_main.dta")

## Part 1
dta$treatment_dummy <- ifelse(dta$edu1849_adult_yos > median(dta$edu1849_adult_yos), 1, 0)

# ensuring dummy is binary at 1 or 0 
stopifnot(dta$treatment_dummy == 1 | dta$treatment_dummy == 0)

# building a matched dataset using genetic matching algo:
matched <- matchit(treatment_dummy ~ pop1849_young + pop1849_old + area1816_qkm + pop1816_cities_pc + 
                     indu1819_texti_pc + steam1849_mining_pc + 
                     vieh1816_schaf_landvieh_pc + occ1816_farm_laborer_t_pc + 
                     buil1816_publ_pc + chausseedummy + trans1816_freight_pc, 
                   data = dta, 
                   method = "genetic")

matched_data <- match.data(matched)

## Part 2

bivar_ols <- lm(fac1849_total_pc ~ treatment_dummy, data = dta)

full_control_ols <- lm(fac1849_total_pc ~ treatment_dummy + 
                         pop1849_young + pop1849_old + 
                         area1816_qkm + pop1816_cities_pc + 
                         indu1819_texti_pc + steam1849_mining_pc + 
                         vieh1816_schaf_landvieh_pc + occ1816_farm_laborer_t_pc + 
                         buil1816_publ_pc + chausseedummy + trans1816_freight_pc, data = dta)

full_control_ols_matched <- lm(fac1849_total_pc ~ treatment_dummy + pop1849_young + 
                                 pop1849_old + area1816_qkm + pop1816_cities_pc + 
                                 indu1819_texti_pc + steam1849_mining_pc + 
                                 vieh1816_schaf_landvieh_pc + occ1816_farm_laborer_t_pc + 
                                 buil1816_publ_pc + chausseedummy + trans1816_freight_pc, data = matched_data)

bivar_iv <- ivreg(fac1849_total_pc ~ treatment_dummy | 
                    edu1816_pri_enrol, data = dta)

full_control_iv <- ivreg(fac1849_total_pc ~ treatment_dummy + pop1849_young + 
                           pop1849_old + area1816_qkm + pop1816_cities_pc + 
                           indu1819_texti_pc + steam1849_mining_pc + 
                           vieh1816_schaf_landvieh_pc + occ1816_farm_laborer_t_pc + 
                           buil1816_publ_pc + chausseedummy + trans1816_freight_pc | 
                           edu1816_pri_enrol + pop1849_young + pop1849_old + area1816_qkm + 
                           pop1816_cities_pc + indu1819_texti_pc + steam1849_mining_pc + 
                           vieh1816_schaf_landvieh_pc + occ1816_farm_laborer_t_pc + 
                           buil1816_publ_pc + chausseedummy + trans1816_freight_pc, data = dta)


## Part 3
stargazer(bivar_ols, full_control_ols, full_control_ols_matched, bivar_iv, full_control_iv,
          ci = TRUE,
          covariate.labels = c("Years of \\\\school > median \\\\in 1849", "Share \\\\pop < 15 years", 
                               "Share \\\\pop > 60 years", "County area \\\\(in 1,000 km2)", 
                               "Share pop living \\\\in cities 1816", "Looms pc 1819", 
                               "Steam engines \\\\in mining pc 1849", "Sheep pc 1816", 
                               "Share farm \\\\laborers 1819", "Public buildings \\\\pc 1821", 
                               "Paved streets \\\\1815 (dummy)", "Tonnage of \\\\ships pc 1819"),
          dep.var.labels = "Share factory workers 1849",
          font.size = "footnotesize",
          column.sep.width = "0pt")
