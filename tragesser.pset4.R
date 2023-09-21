# Connor Tragesser
# The R script is below. Answers to the questions/write ups are in the RMD file and PDF

library(foreign)
library(tidyverse)
library(modelsummary)
library(formatR)
knitr::opts_chunk$set(tidy.opts = list(width.cutoff = 60), tidy = TRUE)
knitr::opts_chunk$set(echo = TRUE)

knitr::opts_knit$set(root.dir = "/Users/connortragesser/Documents/Political Science Coursework Readings/Quant 1/Problem Set 4/")

getwd()
rm(list = ls()) 

# Problem 1
# Part B
# Section A
z = 0
for (x in 10:20) {
  r = (.75^(20-x))*(.25^x)*(choose(20,x))
  z = z + r
}

print(z)

rm(z)

# section B
(.75^10)*(.25^10)*(choose(20,10))
# section C
format(100*((.25^10)*(.75^10)), scientific = FALSE)
# section D
z = 0
draw = 0
for (x in 0:20) {
  r = (.75^(20-x))*(.25^x)*(choose(20,x))
  if (r > z) {
    z = r
    draw = x
  }
}

print(z)
print(draw)

# Problem 4
# Part 1
pnorm(1.8)
# Part 2
pnorm(2) - pnorm(-1.5)
# Part 3
1 - pnorm(3.00)
# Part 4
pnorm(-2.2)
# Part 5
pnorm(1.64) - pnorm(-1.64)
# Problem 5
set.seed(42078)

empty <- matrix(nrow = 10000, ncol = 1)

xdf <- data.frame(empty)
ydf <- data.frame(empty)


xdf$rand <- rnorm(10000, mean = 4, sd = 3)
ydf$rand <- rnorm(10000, mean = 6, sd = 1.5)


hist_maker <- function(dataframe, title) {
  ggplot(dataframe, aes(x = rand)) +
    geom_histogram(aes(y = after_stat(density)), fill = "burlywood2", alpha = 0.8) +
    geom_density(color = "brown3") +
    theme_bw() +
    xlab("Value") + 
    ylab("Density") + 
    ggtitle(title)
}

hist_maker(xdf, "Graph for X Distribution")
hist_maker(ydf, "Graph for Y Distribution")

cat("The quantile value for the X distribution is", quantile(xdf$rand, .95))
cat("The quantile value for the Y distribution is", quantile(ydf$rand, .95))




