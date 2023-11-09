# Connor Tragesser
# See write up/discussants in rmd/pdf

## Gailmard 9.1

### Part A
z = (.51-.5) / ((.5*.5) / 625)^(1/2)
2 * (1 - pnorm(z))

### Part C

lower_bound = -1.96 * ((.51*(1-.51)) / 625)^(1/2) - .51
upper_bound =  1.96 * ((.51*(1-.51)) / 625)^(1/2) - .51

moe = ((upper_bound - lower_bound)/2)*100

moe

## Gailmard 9.2
prop_men_dem = 90/220
n_men = 220
prop_women_dem = 145/243
n_women = 243
n_tot = 220 + 243
tot_prop = (90 + 145) / (220 + 243)

### Part A

lower_bound = -1.96 * sqrt((tot_prop * (1 - tot_prop))/ n_tot)
upper_bound = 1.96 * sqrt((tot_prop * (1 - tot_prop))/ n_tot)

paste("The CI is ", 100 * round(tot_prop + lower_bound, 2), "% to ", 100 *round(tot_prop + upper_bound, 2), "%.", sep = "")

### Part B
z_women = (prop_women_dem-.5) / ((.5*.5) / n_women)^(1/2)
z_men = (prop_men_dem-.5) / ((.5*.5) / n_men)^(1/2)

2 * pnorm(z_men)
2 * (1 - pnorm(z_women))

### Part C
se = sqrt(tot_prop * (1 - tot_prop) * ((1/n_men) + (1/n_women)))
z_pooled_prop = ((prop_men_dem - prop_women_dem))/se
2 * pnorm(z_pooled_prop)

