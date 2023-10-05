# code used for PS6, see discussants/ write up in RMD


library(foreign)
library(tidyverse)
library(modelsummary)
library(formatR)
library(data.table)
setwd("/Users/connortragesser/Documents/Political Science Coursework Readings/Quant 1/Problem Set 6/")


set.seed(428743)

empty5 <- matrix(nrow = 5, ncol = 10000)
empty50 <- matrix(nrow = 50, ncol = 10000)
empty5000 <- matrix(nrow = 5000, ncol = 10000)

dfempty_n5 <- data.frame(empty5)
dfempty_n50 <- data.frame(empty50)
dfempty_n5000 <- data.frame(empty5000)

for(i in colnames(dfempty_n5)) {
  dfempty_n5[[i]] <- rexp(5, rate = 1/20) 
}

for(i in colnames(dfempty_n50)) {
  dfempty_n50[[i]] <- rexp(50, rate = 1/20) 
}

for(i in colnames(dfempty_n5000)) {
  dfempty_n5000[[i]] <- rexp(5000, rate = 1/20) 
}

V1 <- matrix(nrow = 10000, ncol = 1)
df_norm_5 <- data.frame(V1)
df_norm_50 <- data.frame(V1)
df_norm_5000 <- data.frame(V1)

mean_table5 <- dfempty_n5 %>% 
  summarise(across(X1:X10000, mean)) %>% 
  data.table::transpose()

mean_table50 <- dfempty_n50 %>% 
  summarise(across(X1:X10000, mean)) %>% 
  data.table::transpose()

mean_table5000 <- dfempty_n5000 %>% 
  summarise(across(X1:X10000, mean)) %>% 
  data.table::transpose()

df_norm_5$V1 <- rnorm(10000, mean = mean(mean_table5$V1), sd = sd(mean_table5$V1))
df_norm_50$V1 <- rnorm(10000, mean = mean(mean_table50$V1), sd = sd(mean_table50$V1))
df_norm_5000$V1 <- rnorm(10000, mean = mean(mean_table5000$V1), sd = sd(mean_table5000$V1))

graph_maker <- function(exp_dataset, norm_dataset, title) {
  ggplot(exp_dataset, aes(x = V1)) +
    geom_histogram(aes(y = after_stat(density)), fill = "red", alpha = .8) +
    geom_density(data = norm_dataset, color = "blue", alpha = .3) +
    ylab("Density") +
    xlab("Value") +
    ggtitle(title) +
    theme_bw()
}

graph_maker(mean_table5, df_norm_5, "Mean of 10000 draws of 5")
graph_maker(mean_table50, df_norm_50, "Mean of 10000 draws of 50")
graph_maker(mean_table5000, df_norm_5000, "Mean of 10000 draws of 5000")

